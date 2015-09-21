{-# LANGUAGE OverloadedStrings #-}

module TicTacToe
where

import Control.Lens as Lens
import Control.Lens.Traversal

import Data.Text.Lazy as T
import qualified Data.List as L
import Data.Maybe

import Domain

import Test.QuickCheck as Q
import Test.QuickCheck.Arbitrary

orderedValues :: [Value]
orderedValues = L.cycle [X, O]

randomMoves :: IO [Move]
randomMoves = do
    let c = L.map  (\v -> (v `div` 3, v `mod` 3)) [0 .. 8]
    let coordGen = shuffle c :: Gen [(Int, Int)]
    let countGen = Q.elements [0 .. 9] :: Gen Int
    count <- generate countGen
    coords <- generate coordGen
    let pairs = L.zip orderedValues coords
    let full = L.map (\(v, c) -> Move (Coord (fst c)) (Coord (snd c)) v) $ pairs 
    return $ L.take count full

taskQuantity :: Int
taskQuantity = L.length allTasks

testingModule :: TaskId -> [Move] -> [Move] -> Text
testingModule id = renderTask $ lookupTask id

renderTask :: Maybe Task -> [Move] -> [Move] -> Text
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
        dataFunction = T.concat ["message = \"", body, "\""]
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

actionText :: Action -> Text
actionText Validate = "to validate"
actionText Defence  = "to react to"
actionText Winner = "to find out a winner"

printBoard :: [Move] -> Text
printBoard moves = T.concat [
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 result),            "|\n",
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 (L.drop 3 result)), "|\n",
    "+-+-+-+\n",
    "|", T.intercalate "|" (L.take 3 (L.drop 6 result)), "|\n",
    "+-+-+-+"]
    where
        update new old = if old /= empty then "#" else T.pack $ show new
        vals acc [] = acc
        vals acc ((Move (Coord x) (Coord y) v) : t) =
            vals (acc & element coord .~ update v old) t
            where
                coord = x * 3 + y
                old = acc ^?! element coord
        empty = " "
        result = vals (L.take 9 (L.repeat empty)) moves

letters :: [Text]
letters = L.map (T.pack . (:[])) ['a' .. 'z']

data Val = IntVal Int |
    StringVal Text |
    DictVal [(Text, Val)] |
    ListOfVals [Val]
    deriving Show

asArray :: [Move] -> Val
asArray moves = ListOfVals $ L.map toTriple moves
    where
        toValue X = ("v", StringVal "x")
        toValue O = ("v", StringVal "o")
        toTriple (Move (Coord x) (Coord y) v) =
            DictVal [("x", IntVal x), ("y", IntVal y), toValue v]

asMap :: [Move] -> Val
asMap moves = DictVal $ L.zip letters vals
    where
        ListOfVals vals = asArray moves

class ScalaSerialization a where
    renderScala :: a -> Text

instance ScalaSerialization Val where
    renderScala (IntVal i) = T.pack $ show i
    renderScala (StringVal s) = s
    renderScala (DictVal d) = T.concat $
        ["Map(", T.intercalate ", " (L.map (\(k, v) -> T.concat [k, " -> ", renderScala v]) d), ")"]
    renderScala (ListOfVals vs) = T.concat $
        ["List(", T.intercalate ", " (L.map renderScala vs), ")"]

class SExprSerialization a where
    renderSExpr :: a -> Text

instance SExprSerialization Val where
    renderSExpr (IntVal i) = T.pack $ show i
    renderSExpr (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderSExpr (DictVal d) = T.concat $
        ["(m ", T.intercalate " " (L.map (\(k, v) -> T.concat [k, " ", renderSExpr v]) d), ")"]
    renderSExpr (ListOfVals vs) = T.concat $
        ["(l ", T.intercalate " " (L.map renderSExpr vs), ")"]

class MExprSerialization a where
    renderMExpr :: a -> Text

instance MExprSerialization Val where
    renderMExpr (IntVal i) = T.pack $ show i
    renderMExpr (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderMExpr (DictVal d) = T.concat $
        ["m[", T.intercalate "; " (L.map (\(k, v) -> T.concat [k, " ", renderMExpr v]) d), "]"]
    renderMExpr (ListOfVals vs) = T.concat $
        ["l[", T.intercalate "; " (L.map renderMExpr vs), "]"]

class JsonSerialization a where
    renderJson :: a -> Text

instance JsonSerialization Val where
    renderJson (IntVal i) = T.pack $ show i
    renderJson (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderJson (DictVal d) = T.concat $
        ["{", T.intercalate ", " (L.map (\(k, v) -> T.concat ["\\\"", k, "\\\"", ": ", renderJson v]) d), "}"]
    renderJson (ListOfVals vs) = T.concat $
        ["[", T.intercalate ", " (L.map renderJson vs), "]"]

class BencodeSerialization a where
    renderBencode :: a -> Text

instance BencodeSerialization Val where
    renderBencode (IntVal i) = T.concat ["i", T.pack (show i), "e"]
    renderBencode (StringVal s) = T.concat [T.pack (show (T.length s)), ":", s]
    renderBencode (DictVal d) = T.concat $
        ["d", T.concat (L.map (\(k, v) -> T.concat [renderBencode (StringVal k), renderBencode v]) ordered), "e"]
        where
            ordered = L.sortOn (\(t, _) -> t) d
    renderBencode (ListOfVals vs) = T.concat $
        ["l", T.concat (L.map renderBencode vs), "e"]
