{-# LANGUAGE OverloadedStrings #-}

module Serialization

where

import Domain

import Data.Text.Lazy as T
import qualified Data.List as L

-- Scala
renderScala :: WireVal -> Text
renderScala (IntVal i) = T.pack $ show i
renderScala (StringVal s) = s
renderScala (DictVal d) = T.concat $
    ["Map(", T.intercalate ", " (L.map (\(k, v) -> T.concat [k, " -> ", renderScala v]) d), ")"]
renderScala (ListOfVals vs) = T.concat $
    ["List(", T.intercalate ", " (L.map renderScala vs), ")"]

-- S-expression
renderSExpr :: WireVal -> Text
renderSExpr (IntVal i) = T.pack $ show i
renderSExpr (StringVal s) = T.concat ["\"", s, "\""]
renderSExpr (DictVal d) = T.concat $
    ["(m ", T.intercalate " " (L.map (\(k, v) -> T.concat ["\"", k, "\"", " ", renderSExpr v]) d), ")"]
renderSExpr (ListOfVals vs) = T.concat $
    ["(l ", T.intercalate " " (L.map renderSExpr vs), ")"]

-- M-expression
renderMExpr :: WireVal -> Text
renderMExpr (IntVal i) = T.pack $ show i
renderMExpr (StringVal s) = T.concat ["\"", s, "\""]
renderMExpr (DictVal d) = T.concat $
    ["m[", T.intercalate "; " (L.map (\(k, v) -> T.concat ["\"", k, "\"", "; ", renderMExpr v]) d), "]"]
renderMExpr (ListOfVals vs) = T.concat $
    ["l[", T.intercalate "; " (L.map renderMExpr vs), "]"]

-- Json
renderJson :: WireVal -> Text
renderJson (IntVal i) = T.pack $ show i
renderJson (StringVal s) = T.concat ["\"", s, "\""]
renderJson (DictVal d) = T.concat $
    ["{", T.intercalate ", " (L.map (\(k, v) -> T.concat ["\"", k, "\"", ": ", renderJson v]) d), "}"]
renderJson (ListOfVals vs) = T.concat $
    ["[", T.intercalate ", " (L.map renderJson vs), "]"]

-- Bencode
renderBencode :: WireVal -> Text
renderBencode (IntVal i) = T.concat ["i", T.pack (show i), "e"]
renderBencode (StringVal s) = T.concat [T.pack (show (T.length s)), ":", s]
renderBencode (DictVal d) = T.concat $
    ["d", T.concat (L.map (\(k, v) -> T.concat [renderBencode (StringVal k), renderBencode v]) ordered), "e"]
    where
        ordered = L.sortOn (\(t, _) -> t) d
renderBencode (ListOfVals vs) = T.concat $
    ["l", T.concat (L.map renderBencode vs), "e"]
