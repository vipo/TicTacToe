{-# LANGUAGE OverloadedStrings #-}

module Serialization

where

import Domain

import Data.Text.Lazy as T
import qualified Data.List as L

class ScalaSerialization a where
    renderScala :: a -> Text

instance ScalaSerialization WireVal where
    renderScala (IntVal i) = T.pack $ show i
    renderScala (StringVal s) = s
    renderScala (DictVal d) = T.concat $
        ["Map(", T.intercalate ", " (L.map (\(k, v) -> T.concat [k, " -> ", renderScala v]) d), ")"]
    renderScala (ListOfVals vs) = T.concat $
        ["List(", T.intercalate ", " (L.map renderScala vs), ")"]

class SExprSerialization a where
    renderSExpr :: a -> Text

instance SExprSerialization WireVal where
    renderSExpr (IntVal i) = T.pack $ show i
    renderSExpr (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderSExpr (DictVal d) = T.concat $
        ["(m ", T.intercalate " " (L.map (\(k, v) -> T.concat [k, " ", renderSExpr v]) d), ")"]
    renderSExpr (ListOfVals vs) = T.concat $
        ["(l ", T.intercalate " " (L.map renderSExpr vs), ")"]

class MExprSerialization a where
    renderMExpr :: a -> Text

instance MExprSerialization WireVal where
    renderMExpr (IntVal i) = T.pack $ show i
    renderMExpr (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderMExpr (DictVal d) = T.concat $
        ["m[", T.intercalate "; " (L.map (\(k, v) -> T.concat [k, " ", renderMExpr v]) d), "]"]
    renderMExpr (ListOfVals vs) = T.concat $
        ["l[", T.intercalate "; " (L.map renderMExpr vs), "]"]

class JsonSerialization a where
    renderJson :: a -> Text

instance JsonSerialization WireVal where
    renderJson (IntVal i) = T.pack $ show i
    renderJson (StringVal s) = T.concat ["\\\"", s, "\\\""]
    renderJson (DictVal d) = T.concat $
        ["{", T.intercalate ", " (L.map (\(k, v) -> T.concat ["\\\"", k, "\\\"", ": ", renderJson v]) d), "}"]
    renderJson (ListOfVals vs) = T.concat $
        ["[", T.intercalate ", " (L.map renderJson vs), "]"]

class BencodeSerialization a where
    renderBencode :: a -> Text

instance BencodeSerialization WireVal where
    renderBencode (IntVal i) = T.concat ["i", T.pack (show i), "e"]
    renderBencode (StringVal s) = T.concat [T.pack (show (T.length s)), ":", s]
    renderBencode (DictVal d) = T.concat $
        ["d", T.concat (L.map (\(k, v) -> T.concat [renderBencode (StringVal k), renderBencode v]) ordered), "e"]
        where
            ordered = L.sortOn (\(t, _) -> t) d
    renderBencode (ListOfVals vs) = T.concat $
        ["l", T.concat (L.map renderBencode vs), "e"]
