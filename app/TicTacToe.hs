{-# LANGUAGE OverloadedStrings #-}

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
readBoardFromWire (Just "application/bencode+list") b = tpr readBencode fromArray b 
readBoardFromWire (Just "application/json+list") b = tpr readJson fromArray b
readBoardFromWire (Just "application/s-expr+list") b = tpr readSExpr fromArray b
readBoardFromWire (Just "application/m-expr+list") b = tpr readMExpr fromArray b
readBoardFromWire (Just "application/scala+list") b = tpr readScala fromArray b
readBoardFromWire (Just "application/bencode+map") b = tpr readBencode fromMap b
readBoardFromWire (Just "application/json+map") b = tpr readJson fromMap b
readBoardFromWire (Just "application/s-expr+map") b = tpr readSExpr fromMap b
readBoardFromWire (Just "application/m-expr+map") b = tpr readMExpr fromMap b
readBoardFromWire (Just "application/scala+map") b = tpr readScala fromMap b
readBoardFromWire _ _ = Left (415, toBS "Unsupported Media Type", BSL.empty)

tpr :: (T.Text -> Either T.Text WireVal)
    -> (WireVal -> Maybe Moves) -> BSL.ByteString
    -> Either (Int, BS.ByteString, BSL.ByteString) Moves
tpr reader transformer bytes =
    case reader (TLE.decodeUtf8 bytes) of
        Left err -> Left (400, badRequest, TLE.encodeUtf8 err)
        Right w -> case transformer w of
                    Nothing -> Left (400, badRequest, illegalFormat)
                    Just m -> Right m

toBS :: String -> BS.ByteString
toBS = BS.pack . UTF.encode

toBSL :: String -> BSL.ByteString
toBSL = BSL.pack . UTF.encode

badRequest :: BS.ByteString
badRequest = toBS "Bad request"

illegalFormat :: BSL.ByteString
illegalFormat = toBSL "Illegal format, i.e. got array instead of map"
