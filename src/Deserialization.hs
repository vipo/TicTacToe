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
parseIntVal :: Parser WireVal
parseIntVal = (many1 digit >>= (\v -> return (IntVal (read v))))

withOutSpaces :: T.Text -> T.Text
withOutSpaces = T.filter (\c -> not (c `elem` [' ', '\n', '\r', '\t']))

parseQuotedString :: Parser T.Text
parseQuotedString = do
    _ <- string "\""
    s <- many alphaNum
    _ <- string "\""
    return $ T.pack s

parseQuotedStringVal :: Parser WireVal
parseQuotedStringVal = do
    s <- parseQuotedString
    return $ StringVal s

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
    i <- parseIntVal
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
    mapLeft (\err -> T.pack (show err)) $ parse parseJson "" $ withOutSpaces msg

parseJson :: Parser WireVal
parseJson = parseJsonList
    <|> parseJsonDict
    <|> parseIntVal
    <|> parseQuotedStringVal

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

-- s-expr

readSExpr :: T.Text -> Either T.Text WireVal
readSExpr msg =
    mapLeft (\err -> T.pack (show err)) $ parse parseSExpr "" msg

parseSExpr :: Parser WireVal
parseSExpr = parseNested
    <|> parseIntVal
    <|> parseQuotedStringVal

parseNested :: Parser WireVal
parseNested = do
    _ <- string "("
    ds <- parseStartedSExprList <|>  parseStartedSExprDict
    return ds

parseStartedSExprList :: Parser WireVal
parseStartedSExprList = do
    _ <- string "l"
    _ <- many space
    t <- many spaceSeparated
    _ <- string ")"
    return $ ListOfVals t
    where
        spaceSeparated = do
            v <- parseSExpr
            _ <- many space
            return v

parseStartedSExprDict :: Parser WireVal
parseStartedSExprDict = do
    _ <- string "m"
    _ <- many space
    t <- many spaceSeparated
    _ <- string ")"
    return $ DictVal t
    where
        spaceSeparated = do
            k <- parseQuotedString
            _ <- many space
            v <- parseSExpr
            _ <- many space
            return (k, v)

-- scala

readScala :: T.Text -> Either T.Text WireVal
readScala msg =
    mapLeft (\err -> T.pack (show err)) $ parse parseScala "" $ withOutSpaces msg

parseScala :: Parser WireVal
parseScala = parseScalaList
    <|> parseScalaDict
    <|> parseScalaValue

parseScalaValue :: Parser WireVal
parseScalaValue = try parseIntVal <|> parseScalaStr

parseScalaStr :: Parser WireVal
parseScalaStr = do
    s <- many1 alphaNum
    return $ StringVal $ T.pack s

parseScalaList :: Parser WireVal
parseScalaList = do
    _ <- string "List("
    h <- optionMaybe parseScala
    t <- many commaSeparated
    _ <- string ")"
    return $ ListOfVals $ (Maybe.maybeToList h) ++ t
    where
        commaSeparated = do
            _ <- string ","
            v <- parseScala
            return v

parseScalaDict :: Parser WireVal
parseScalaDict = do
    _ <- string "Map("
    h <- optionMaybe pair
    t <- many commaSeparated
    _ <- string ")"
    return $ DictVal $ (Maybe.maybeToList h) ++ t
    where
        pair = do
            k <- many1 alphaNum
            _ <- string "->"
            v <- parseScala
            return (T.pack k, v)
        commaSeparated = do
            _ <- string ","
            v <- pair
            return v
