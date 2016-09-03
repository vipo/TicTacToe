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
      QC.testProperty "array of maps"    $ testArrayOfMaps   readBencode renderBencode
    , QC.testProperty "map of maps"      $ testMapOfMaps     readBencode renderBencode
    , QC.testProperty "arrays of arrays" $ testArrayOfArrays readBencode renderBencode
    , testCase "some list" $ readBencode "li42e4:1234e"
        @?= Right (ListOfVals [IntVal 42, StringVal "1234"])
    , testCase "some map" $ readBencode "d4:1234i42ee"
        @?= Right (DictVal [("1234", IntVal 42)])
    , testCase "some error" $ isLeft (readBencode "d4:1234i42e")
        @?= True
  ]

json :: TestTree
json = testGroup "Json" [
      QC.testProperty "array of maps"    $ testArrayOfMaps   readJson renderJson
    , QC.testProperty "map of maps"      $ testMapOfMaps     readJson renderJson
    , QC.testProperty "arrays of arrays" $ testArrayOfArrays readJson renderJson
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
      QC.testProperty "array of maps"    $ testArrayOfMaps   readMExpr renderMExpr
    , QC.testProperty "map of maps"      $ testMapOfMaps     readMExpr renderMExpr
    , QC.testProperty "arrays of arrays" $ testArrayOfArrays readMExpr renderMExpr
    , testCase "some list" $ readMExpr "l[ 42 ;\"a1234\"]"
        @?= Right (ListOfVals [IntVal 42, StringVal "a1234"])
    , testCase "empty list" $ readMExpr "l[]"
        @?= Right (ListOfVals [])
    , testCase "one element list" $ readMExpr "l[ 42]"
        @?= Right (ListOfVals [IntVal 42])
    , testCase "some map" $ readMExpr "m[\"1234\"; 42; \"1\";\"a1\"] "
        @?= Right (DictVal [("1234", IntVal 42), ("1", StringVal "a1")])
    , testCase "empty map" $ readMExpr "m[]"
        @?= Right (DictVal [])
    , testCase "one element map" $ readMExpr "m[\"1\"; l[]]"
        @?= Right (DictVal [("1", ListOfVals [])])
    , testCase "some error" $ isLeft (readMExpr "m[42]")
        @?= True
  ]

sexpr :: TestTree
sexpr = testGroup "SExpr" [
      QC.testProperty "array of maps"    $ testArrayOfMaps   readSExpr renderSExpr
    , QC.testProperty "map of maps"      $ testMapOfMaps     readSExpr renderSExpr
    , QC.testProperty "arrays of arrays" $ testArrayOfArrays readSExpr renderSExpr
    , testCase "some list" $ readSExpr "(l 42 \"a1234\")"
        @?= Right (ListOfVals [IntVal 42, StringVal "a1234"])
    , testCase "empty list" $ readSExpr "(l  )"
        @?= Right (ListOfVals [])
    , testCase "one element list" $ readSExpr "(l 42)"
        @?= Right (ListOfVals [IntVal 42])
    , testCase "some map" $ readSExpr "(m \"1234\" 42 \"1\" \"a1\") "
        @?= Right (DictVal [("1234", IntVal 42), ("1", StringVal "a1")])
    , testCase "empty map" $ readSExpr "(m)"
        @?= Right (DictVal [])
    , testCase "one element map" $ readSExpr "(m \"1\" (l))"
        @?= Right (DictVal [("1", ListOfVals [])])
    , testCase "some error" $ isLeft (readSExpr "(m 42)")
        @?= True
  ]

scala :: TestTree
scala = testGroup "Scala" [
      QC.testProperty "array of maps"    $ testArrayOfMaps   readScala renderScala
    , QC.testProperty "map of maps"      $ testMapOfMaps     readScala renderScala
    , QC.testProperty "arrays of arrays" $ testArrayOfArrays readScala renderScala
    , testCase "some list" $ readScala "List(42, a1234)"
        @?= Right (ListOfVals [IntVal 42, StringVal "a1234"])
    , testCase "empty list" $ readScala "List()"
        @?= Right (ListOfVals [])
    , testCase "one element list" $ readScala "List(42)"
        @?= Right (ListOfVals [IntVal 42])
    , testCase "some map" $ readScala "Map (1234 -> 42, 1 -> a1) "
        @?= Right (DictVal [("1234", IntVal 42), ("1", StringVal "a1")])
    , testCase "empty map" $ readScala "Map ( )"
        @?= Right (DictVal [])
    , testCase "one element map" $ readScala "Map (1 -> List() )"
        @?= Right (DictVal [("1", ListOfVals [])])
    , testCase "some error" $ isLeft (readScala "Map(42)")
        @?= True
  ]

testArrayOfMaps :: (T.Text -> Either T.Text WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testArrayOfMaps reader renderer moves =
  fromJust (fromArrayOfMaps (unsafe (reader (renderer (asArrayOfMaps moves))))) == moves

testMapOfMaps :: (T.Text -> Either T.Text WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testMapOfMaps reader renderer moves =
  fromJust (fromMapOfMaps (unsafe (reader (renderer (asMapOfMaps moves))))) == moves

testArrayOfArrays :: (T.Text -> Either T.Text WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testArrayOfArrays reader renderer moves =
  fromJust (fromArrayOfArrays (unsafe (reader (renderer (asArrayOfArrays moves))))) == moves

unsafe :: Either T.Text WireVal -> WireVal
unsafe result = case result of
  Right w -> w
  Left msg -> error $ T.unpack msg
