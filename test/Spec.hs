{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import Data.Maybe
import Data.Either

import qualified Data.Text.Lazy as T

import Domain
import Serialization
import Deserialization

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "TicTacToe Specification" [bencode, json, mexpr, sexpr, scala]

bencode :: TestTree
bencode = testGroup "Bencode" [
    QC.testProperty "array" $ testArray readBencode renderBencode
    , QC.testProperty "map" $ testMap readBencode renderBencode
    , testCase "some list" $ readBencode "li42e4:1234e"
        @?= Right (ListOfVals [IntVal 42, StringVal "1234"])
    , testCase "some map" $ readBencode "d4:1234i42ee"
        @?= Right (DictVal [("1234", IntVal 42)])
    , testCase "some error" $ isLeft (readBencode "d4:1234i42e")
        @?= True
  ]

json :: TestTree
json = testGroup "Json" [
    QC.testProperty "array" $ testArray readJson renderJson
    , QC.testProperty "map" $ testMap readJson renderJson
    , testCase "some list" $ readJson "[42, \"1234\"]"
        @?= Right (ListOfVals [IntVal 42, StringVal "1234"])
    , testCase "empty list" $ readJson "[]"
        @?= Right (ListOfVals [])
    , testCase "one element list" $ readJson "[42]"
        @?= Right (ListOfVals [IntVal 42])
    , testCase "some map" $ readJson "{ \"1234\" : 42, \"1\": \"1\"} "
        @?= Right (DictVal [("1234", IntVal 42), ("1", StringVal "1")])
    , testCase "empty map" $ readJson "{  }"
        @?= Right (DictVal [])
    , testCase "one element map" $ readJson "{ \"1\": [] }"
        @?= Right (DictVal [("1", ListOfVals [])])
    , testCase "some error" $ isLeft (readJson "{42,}")
        @?= True
  ]

mexpr :: TestTree
mexpr = testGroup "MExpr" [
    QC.testProperty "array" $ testArray readMExpr renderMExpr
    , QC.testProperty "map" $ testMap readMExpr renderMExpr
  ]

sexpr :: TestTree
sexpr = testGroup "SExpr" [
    QC.testProperty "array" $ testArray readSExpr renderSExpr 
    , QC.testProperty "map" $ testMap readSExpr renderSExpr
  ]

scala :: TestTree
scala = testGroup "Scala" [
    QC.testProperty "array" $ testArray readScala renderScala
    , QC.testProperty "map" $ testMap readScala renderScala
  ]

testArray :: (T.Text -> Either T.Text WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testArray reader renderer moves =
  fromJust (fromArray (unsafe (reader (renderer (asArray moves))))) == moves

testMap :: (T.Text -> Either T.Text WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testMap reader renderer moves =
  fromJust (fromMap (unsafe (reader (renderer (asMap moves))))) == moves

unsafe :: Either T.Text WireVal -> WireVal
unsafe result = case result of
  Right w -> w
  Left msg -> error $ T.unpack msg
