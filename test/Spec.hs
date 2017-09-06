{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import           Data.Either
import           Data.Maybe

import qualified Data.Text.Lazy        as T

import           Deserialization
import           Domain
import           Serialization

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "TicTacToe Specification" [
    commons, bencode, json
  ]

commons :: TestTree
commons = testGroup "Common properties" [
    testCase "number of tasks" $ 48 @?= length allTasks
  ]

bencode :: TestTree
bencode = testGroup "Bencode" [
      QC.testProperty "array of maps"               $ testArrayOfMaps   readBencode renderBencode
    , QC.testProperty "map of maps"                 $ testMapOfMaps     readBencode renderBencode
    , QC.testProperty "arrays of arrays"            $ testArrayOfArrays readBencode renderBencode
    , testCase "some list" $ readBencode "li42e4:1234e"
        @?= Right (ListOfVals [IntVal 42, StringVal "1234"])
    , testCase "some map" $ readBencode "d4:1234i42ee"
        @?= Right (DictVal [("1234", IntVal 42)])
    , testCase "some error" $ isLeft (readBencode "d4:1234i42e")
        @?= True
  ]

json :: TestTree
json = testGroup "Json" [
      QC.testProperty "array of maps"              $ testArrayOfMaps   readJson renderJson
    , QC.testProperty "map of maps"                $ testMapOfMaps     readJson renderJson
    , QC.testProperty "arrays of arrays"           $ testArrayOfArrays readJson renderJson
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
  Right w  -> w
  Left msg -> error $ T.unpack msg
