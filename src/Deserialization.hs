{-# LANGUAGE OverloadedStrings #-}

module Deserialization(
    readBencode, readJson, readMExpr,
    readSExpr, readScala
)

where

import Domain

import Text.Parsec.Text.Lazy
import Text.Parsec

import Data.Either.Combinators
import qualified Data.Text.Lazy as T

-- commons
parseInt :: Parser WireVal
parseInt = (many1 digit >>= (\v -> return (IntVal (read v))))

-- bencode

readBencode :: T.Text -> Either T.Text WireVal
readBencode msg = mapLeft (\err -> T.pack (show err)) $ parse parseBencode "" msg

parseBencode :: Parser WireVal
parseBencode = parseBencodeInt
    <|> parseBencodeList
    <|> parseBencodeDict
    <|> parseBencodeStr

parseBencodeInt :: Parser WireVal
parseBencodeInt = do
    _ <- string "i"
    i <- parseInt
    _ <- string "e"
    return i

parseBencodeStr :: Parser WireVal
parseBencodeStr = do
    l <- many1 digit
    _ <- string ":"
    s <- count (read l) anyChar
    return $ StringVal $ T.pack s

parseBencodeList :: Parser WireVal
parseBencodeList = do
    _ <- string "l"
    l <- many parseBencode
    _ <- string "e"
    return $ ListOfVals l

parseBencodeDict :: Parser WireVal
parseBencodeDict = do
    _ <- string "d"
    e <- many parseBencodeDictEntry
    _ <- string "e"
    return $ DictVal e
    where
        parseBencodeDictEntry = do
            StringVal k <- parseBencodeStr
            v <- parseBencode
            return (k, v)

-- json

readJson :: T.Text -> Either T.Text WireVal
readJson _ = Left "error"

readMExpr :: T.Text -> Either T.Text WireVal
readMExpr _ = Left "error"

readSExpr :: T.Text -> Either T.Text WireVal
readSExpr _ = Left "error"

readScala :: T.Text -> Either T.Text WireVal
readScala _ = Left "error"
