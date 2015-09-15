{-# LANGUAGE OverloadedStrings #-}

module TicTacToe
where

import Data.Text.Lazy as T
import Data.List as L

import Domain

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

infinitRandomMoves :: IO [Move]
infinitRandomMoves =
    let list = infiniteList :: Gen [Move]
    in generate list

taskQuantity :: Int
taskQuantity = L.length allTasks

testingModule :: TaskId -> [Move] -> Text
testingModule id moves = renderTask (lookupTask id) moves

renderTask :: Maybe Task -> [Move] -> Text
renderTask Nothing _ = ""
renderTask (Just (action, format, modifier)) m =
    let moduleName = T.concat ["module TicTacToe.Messages.", T.pack (show format), "\nwhere\n\n"]
        moves = L.take 9 m
        renderer = case format of
            Scala -> renderScala
            SExpr -> renderSExpr
            MExpr -> renderMExpr
            Json -> renderJson
            Bencode -> renderBencode
        array = asArray moves
        dict = asMap moves
        body = case modifier of
            AsIs -> renderer array
            NoArrays -> renderer dict
        dataSignature = "message :: String\n"
        dataFunction = T.concat ["message = \"", body, "\""]
    in T.concat [moduleName, dataSignature, dataFunction, "\n"]

letters :: [Text]
letters = L.map (T.pack . (:[])) ['a' .. 'z']

data Val = IntVal Integer |
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
