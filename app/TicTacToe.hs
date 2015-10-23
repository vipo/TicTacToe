{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module TicTacToe
where

import Control.Lens as Lens

import Deserialization

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List as L
import qualified TextShow as TS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Binary.UTF8.String as UTF
import Data.Maybe

import qualified Database.Redis as R

import Domain
import Serialization

import Test.QuickCheck as Q

orderedValues :: [Value]
orderedValues = L.cycle [X, O]

randomMoves :: IO [Move]
randomMoves = do
    let allCoords = L.map  (\v -> (v `div` 3, v `mod` 3)) [0 .. 8]
    let coordGen = shuffle allCoords :: Gen [(Int, Int)]
    let countGen = Q.elements [0 .. 9] :: Gen Int
    count <- generate countGen
    coords <- generate coordGen
    let pairs = L.zip orderedValues coords
    let full = L.map (\(v, c) -> Move (Coord (fst c)) (Coord (snd c)) v) $ pairs 
    return $ L.take count full

taskQuantity :: Int
taskQuantity = L.length allTasks

testingModule :: TaskId -> [Move] -> [Move] -> T.Text
testingModule taskId = renderTask $ lookupTask taskId

renderTask :: Maybe Task -> [Move] -> [Move] -> T.Text
renderTask Nothing _ _ = ""
renderTask (Just (action, format, modifier)) mandatoryMoves extraMoves =
    let moduleName = T.concat ["module TicTacToe.Messages.", T.pack (show format), "\nwhere\n\n"]
        renderer = case format of
            Scala -> renderScala
            SExpr -> renderSExpr
            MExpr -> renderMExpr
            Json -> renderJson
            Bencode -> renderBencode
        moves = case action of
            Validate -> (L.take 8 mandatoryMoves) ++ (L.take 1 extraMoves)
            _ -> case L.span (not . thereIsWinner) (L.inits mandatoryMoves) of
                (a@(_ : _), []) -> L.head $ L.reverse a
                (_, h : _) -> h
                _ -> []
        body = case modifier of
            AsIs -> renderer $ asArray moves
            NoArrays -> renderer $ asMap moves
        dataComment = T.concat ["{-\nmessage ", actionText action, "\nboard:\n", printBoard moves, "\n-}\n"]
        dataSignature = "message :: String\n"
        dataFunction = T.concat ["message = ", TS.showtl body]
    in T.concat [moduleName, dataComment, dataSignature, dataFunction, "\n"]

thereIsWinner :: [Move] -> Bool
thereIsWinner [] = False
thereIsWinner ms =
    case movesToVal ms of
        [a, b, c, _, _, _, _, _, _] | isJust a && a == b && b == c -> True
        [_, _, _, a, b, c, _, _, _] | isJust a && a == b && b == c -> True
        [_, _, _, _, _, _, a, b, c] | isJust a && a == b && b == c -> True
        [a, _, _, b, _, _, c, _, _] | isJust a && a == b && b == c -> True
        [_, a, _, _, b, _, _, c, _] | isJust a && a == b && b == c -> True
        [_, _, a, _, _, b, _, _, c] | isJust a && a == b && b == c -> True
        [a, _, _, _, b, _, _, _, c] | isJust a && a == b && b == c -> True
        [_, _, a, _, b, _, c, _, _] | isJust a && a == b && b == c -> True
        _ -> False

movesToVal :: [Move] -> [Maybe Value]
movesToVal ms = L.foldl toVal initial ms
    where
        initial = L.take 9 $ L.repeat Nothing
        toVal acc (Move (Coord x) (Coord y) v) = acc & element (x * 3 + y) .~ Just v

actionText :: Action -> T.Text
actionText Validate = "to validate"
actionText Defence  = "to react to"
actionText Winner = "to find out a winner"

printBoard :: [Move] -> T.Text
printBoard moves = T.concat [
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 result),            "|\n",
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 (L.drop 3 result)), "|\n",
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 (L.drop 6 result)), "|\n",
    "+-+-+-+"]
    where
        update new old = if old /= emptyCell then "#" else T.pack $ show new
        vals acc [] = acc
        vals acc ((Move (Coord x) (Coord y) v) : t) =
            vals (acc & element coord .~ update v old) t
            where
                coord = x * 3 + y
                old = acc ^?! element coord
        emptyCell = " "
        result = vals (L.take 9 (L.repeat emptyCell)) moves

readBoardFromWire :: Maybe T.Text -> BSL.ByteString -> Either (Int, BS.ByteString, BSL.ByteString) Moves
readBoardFromWire (Just "application/bencode+list") = tpr readBencode fromArray
readBoardFromWire (Just "application/json+list") = tpr readJson fromArray
readBoardFromWire (Just "application/s-expr+list") = tpr readSExpr fromArray
readBoardFromWire (Just "application/m-expr+list") = tpr readMExpr fromArray
readBoardFromWire (Just "application/scala+list") = tpr readScala fromArray
readBoardFromWire (Just "application/bencode+map") = tpr readBencode fromMap
readBoardFromWire (Just "application/json+map") = tpr readJson fromMap
readBoardFromWire (Just "application/s-expr+map") = tpr readSExpr fromMap
readBoardFromWire (Just "application/m-expr+map") = tpr readMExpr fromMap
readBoardFromWire (Just "application/scala+map") = tpr readScala fromMap
readBoardFromWire _ = (\_ -> Left (415, toBS "Unsupported Media Type", BSL.empty))

tpr :: (T.Text -> Either T.Text WireVal)
    -> (WireVal -> Maybe Moves) -> BSL.ByteString
    -> Either (Int, BS.ByteString, BSL.ByteString) Moves
tpr reader transformer bytes =
    case reader (TLE.decodeUtf8 bytes) of
        Left err -> Left (400, badRequest, TLE.encodeUtf8 err)
        Right w -> case transformer w of
                    Nothing -> Left (400, badRequest, illegalFormat)
                    Just m -> Right m

record :: R.Connection -> Moves -> T.Text -> IO (Int, BS.ByteString, BSL.ByteString)
record conn moves gameId = do
    state <- currentState conn gameId
    case state of
        Right (len, gameNum) | len < 9 -> do
            saveMoves conn gameId moves gameNum
            return (200, "OK", BSL.empty)
        _ -> return (500, "Internal Server Error", "The game is finished")

toBS :: String -> BS.ByteString
toBS = BS.pack . UTF.encode

toBSL :: String -> BSL.ByteString
toBSL = BSL.pack . UTF.encode

lt2bs :: T.Text -> BS.ByteString
lt2bs = BSL.toStrict . TLE.encodeUtf8

badRequest :: BS.ByteString
badRequest = toBS "Bad request"

illegalFormat :: BSL.ByteString
illegalFormat = toBSL "Illegal format, i.e. got array instead of map"

-- Redis stuff

channelKey :: T.Text -> BS.ByteString
channelKey gameId = lt2bs $ T.concat ["channel:", gameId]

historyKey :: T.Text -> BS.ByteString
historyKey gameId = lt2bs $ T.concat ["history:", gameId]

currentState :: R.Connection -> T.Text -> IO (Either R.Reply (Integer, Integer))
currentState conn gameId = R.runRedis conn $ do
    len <- R.llen $ historyKey gameId
    count <- R.incr "counter"
    return $ [ (l, c) | l <- len, c <- count ]

saveMoves :: R.Connection -> T.Text -> Moves -> Integer -> IO ()
saveMoves conn gameId moves moveNum = R.runRedis conn $ do
    let asJson = renderJson $ asArray moves
    let d = map lt2bs [asJson]
    _ <- R.zadd "games" [(fromIntegral moveNum, lt2bs gameId)]
    _ <- R.rpush (historyKey gameId) d 
    _ <- R.rpush (channelKey gameId) d
    return ()