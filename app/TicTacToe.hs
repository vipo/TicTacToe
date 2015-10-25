{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module TicTacToe
where

import Control.Lens as Lens
import Control.Monad (forM_)
import Deserialization

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List as L
import qualified TextShow as TS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.Char8 as C8
import Data.Maybe

import qualified Data.Map.Strict as Map

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

transformers :: Map.Map T.Text (
        (T.Text -> Either T.Text WireVal),
        (WireVal -> Maybe Moves),
        ([Move] -> WireVal),
        (WireVal -> T.Text))
transformers = Map.fromList [
        ("application/bencode+list", (readBencode, fromArray, asArray, renderBencode))
        , ("application/json+list", (readJson, fromArray, asArray, renderJson))
        , ("application/s-expr+list", (readSExpr, fromArray, asArray, renderSExpr))
        , ("application/m-expr+list", (readMExpr, fromArray, asArray, renderMExpr))
        , ("application/scala+list", (readScala, fromArray, asArray, renderScala))
        , ("application/bencode+map", (readBencode, fromMap, asMap, renderBencode))
        , ("application/json+map", (readJson, fromMap, asMap, renderJson))
        , ("application/s-expr+map", (readSExpr, fromMap, asMap, renderSExpr))
        , ("application/m-expr+map", (readMExpr, fromMap, asMap, renderMExpr))
        , ("application/scala+map", (readScala, fromMap, asMap, renderScala))
    ]

plainText :: T.Text
plainText = "text/plain; charset=UTF-8"

retrieveMove :: R.Connection -> Maybe T.Text -> GameId -> Player -> IO (Int, BS.ByteString, BSL.ByteString, T.Text)
retrieveMove conn acc gameId playerId =
    case (acc >>= (\x -> Map.lookup x transformers >>= (\t -> return (x, t)))) of
        Nothing -> return (415, toBS "Unsupported Media Type", toBSL "Unknown Accept header", plainText)
        Just (ct, (_, _, tr, ren)) -> do
            raw <- readFromChannel conn gameId playerId
            case raw of
                Nothing -> return (500, "Internal Server Error", toBSL "No move data available at the moment", plainText)
                Just d -> do
                    let resp = ren $ tr $ readJsonOptimistically $ bs2lt d
                    return (200, "OK", TLE.encodeUtf8 resp, ct)

readBoardFromWire :: Maybe T.Text -> BSL.ByteString -> Either (Int, BS.ByteString, BSL.ByteString) Moves
readBoardFromWire ct body =
    case (ct >>= (\x -> Map.lookup x transformers)) of
        Nothing -> Left (415, toBS "Unsupported Media Type", BSL.empty)
        Just (reader, ds, _, _) -> tpr reader ds body

tpr :: (T.Text -> Either T.Text WireVal)
    -> (WireVal -> Maybe Moves) -> BSL.ByteString
    -> Either (Int, BS.ByteString, BSL.ByteString) Moves
tpr reader transformer bytes =
    case reader (TLE.decodeUtf8 bytes) of
        Left err -> Left (400, badRequest, TLE.encodeUtf8 err)
        Right w -> case transformer w of
                    Nothing -> Left (400, badRequest, illegalFormat)
                    Just m -> Right m

data Player = Player1 | Player2

data GameId = GameId ! T.Text

oppositePlayer :: Player -> Player
oppositePlayer Player1 = Player2
oppositePlayer Player2 = Player1

record :: R.Connection -> Moves -> GameId -> Player -> IO (Int, BS.ByteString, BSL.ByteString)
record conn moves gameId playerId = do
    state <- currentState conn gameId
    case state of
        Right (len, gameNum) | len < 9 -> do
            saveMoves conn gameId playerId moves gameNum
            return (200, "OK", BSL.empty)
        _ -> return (500, "Internal Server Error", "The game is finished")

gameHistory :: R.Connection -> GameId -> IO T.Text
gameHistory conn gameId = do
    his <- history conn gameId
    let nums = [1..] :: [Integer]
    let ind = zip (map show nums) $ map (printBoard . readJsonOptimistically . bs2lt) his
    return $ T.intercalate "\n" $ map (\(i, d) -> T.concat ["Move ", T.pack i, ":\n", d]) ind

readJsonOptimistically :: T.Text -> Moves
readJsonOptimistically t = case readJson t of
                            Right v -> fromMaybe [] $ fromArray v
                            Left _ -> []

gameTop :: R.Connection -> IO T.Text
gameTop conn = do
    gameIds <- lastGames conn
    let html = H.docTypeHtml $ do
            H.head $ do
                H.title "History"
            H.body $ do
                H.h1 "Last games"
                H.ol $ forM_ (map (\(GameId g) -> g) gameIds) toLink
    return $ renderHtml html
    where
        toLink g = H.li $ H.a ! A.href (H.toValue $ T.concat ["/history/", g]) $ H.toHtml g

toBS :: String -> BS.ByteString
toBS = BS.pack . UTF.encode

toBSL :: String -> BSL.ByteString
toBSL = BSL.pack . UTF.encode

lt2bs :: T.Text -> BS.ByteString
lt2bs = BSL.toStrict . TLE.encodeUtf8

bs2lt :: BS.ByteString -> T.Text
bs2lt = TLE.decodeUtf8 . BSL.fromStrict

badRequest :: BS.ByteString
badRequest = toBS "Bad request"

illegalFormat :: BSL.ByteString
illegalFormat = toBSL "Illegal format, i.e. got array instead of map"

history :: R.Connection -> GameId -> IO [BS.ByteString]
history conn gameId = R.runRedis conn $ do
    els <- R.lrange (historyKey gameId) 0 8
    ret <- case els of 
            Right r -> return r
            Left _ -> return []
    return ret

channelKey :: GameId -> Player -> BS.ByteString
channelKey (GameId gameId) playerId =
    lt2bs $ T.concat ["channel:", gameId, ":", playerString playerId]
    where
        playerString Player1 = "1"
        playerString Player2 = "2"

historyKey :: GameId -> BS.ByteString
historyKey (GameId gameId) = lt2bs $ T.concat ["history:", gameId]

counterKey :: BS.ByteString
counterKey = "counter"

gamesKey :: BS.ByteString
gamesKey = "games"

currentState :: R.Connection -> GameId -> IO (Either R.Reply (Integer, Integer))
currentState conn gameId = R.runRedis conn $ do
    len <- R.llen $ historyKey gameId
    count <- R.incr counterKey
    return $ [ (l, c) | l <- len, c <- count ]

saveMoves :: R.Connection -> GameId -> Player -> Moves -> Integer -> IO ()
saveMoves conn gameId playerId moves moveNum = R.runRedis conn $ do
    let asJson = renderJson $ asArray moves
    let d = map lt2bs [asJson]
    _ <- R.multiExec $ do
        _ <- R.zadd gamesKey [(fromIntegral moveNum, rawGameId gameId)]
        _ <- R.rpush (historyKey gameId) d
        _ <- R.rpush (channelKey gameId (oppositePlayer playerId)) d
        return $ return ()
    return ()
    where
        rawGameId (GameId v) = lt2bs v

readFromChannel :: R.Connection -> GameId -> Player -> IO (Maybe BS.ByteString)
readFromChannel conn gameId playerId = do
    v <- R.runRedis conn $ R.blpop [channelKey gameId playerId] 5
    case v of
        Right (Just (_, b)) -> return $ Just b
        _ -> return Nothing

lastGames :: R.Connection -> IO [GameId]
lastGames conn = R.runRedis conn $ do
    c <- R.get counterKey
    let counter = case c of
                    Right (Just v) -> read $ C8.unpack v
                    _ -> 0 :: Integer
    range <- R.zrevrangebyscoreLimit gamesKey (fromIntegral counter) (
        fromIntegral (0 :: Integer)) 0 30
    case range of
        Left _ -> return []
        Right r -> return $ map (GameId . bs2lt) r
