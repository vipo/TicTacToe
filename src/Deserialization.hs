{-# LANGUAGE OverloadedStrings #-}

module Deserialization(
    readBencode, readJson, readMExpr,
    readSExpr, readScala
)

where

import Domain

import Text.Parsec.Text.Lazy
import Text.Parsec

import qualified Data.Maybe as Maybe
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
readJson msg =
    let wospaces = T.filter (\c -> not (c `elem` [' ', '\n', '\r', '\t'])) msg
    in mapLeft (\err -> T.pack (show err)) $ parse parseJson "" wospaces

parseJson :: Parser WireVal
parseJson = parseJsonList
    <|> parseJsonDict
    <|> parseInt
    <|> parseJsonStr

parseQuotedString :: Parser T.Text
parseQuotedString = do
    _ <- string "\""
    s <- many alphaNum
    _ <- string "\""
    return $ T.pack s

parseJsonStr :: Parser WireVal
parseJsonStr = do
    s <- parseQuotedString
    return $ StringVal s

parseJsonList :: Parser WireVal
parseJsonList = do
    _ <- string "["
    h <- optionMaybe parseJson
    t <- many commaSeparated
    _ <- string "]"
    return $ ListOfVals $ (Maybe.maybeToList h) ++ t
    where
        commaSeparated = do
            _ <- string ","
            v <- parseJson
            return v


parseJsonDict :: Parser WireVal
parseJsonDict = do
    _ <- string "{"
    h <- optionMaybe pair
    t <- many commaSeparated
    _ <- string "}"
    return $ DictVal $ (Maybe.maybeToList h) ++ t
    where
        pair = do
            k <- parseQuotedString
            _ <- string ":"
            v <- parseJson
            return (k, v)
        commaSeparated = do
            _ <- string ","
            v <- pair
            return v


-- m-expr

readMExpr :: T.Text -> Either T.Text WireVal
readMExpr _ = Left "error"

readSExpr :: T.Text -> Either T.Text WireVal
readSExpr _ = Left "error"

readScala :: T.Text -> Either T.Text WireVal
readScala _ = Left "error"
