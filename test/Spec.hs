{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import Data.Maybe

import qualified Data.Text.Lazy as T

import Domain
import Serialization
import Deserialization

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "TicTacToe Specification" [bencode, json, mexpr, sexpr, scala]

bencode :: TestTree
bencode = testGroup "Bencode" [
    QC.testProperty "array" $ testArray readBencode renderBencode
    , QC.testProperty "map" $ testMap readBencode renderBencode
  ]

json :: TestTree
json = testGroup "Json" [
    QC.testProperty "array" $ testArray readJson renderJson
    , QC.testProperty "map" $ testMap readJson renderJson
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

testArray :: (T.Text -> WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testArray reader renderer moves = 
  fromJust (fromArray (reader (renderer (asArray moves)))) == moves

testMap :: (T.Text -> WireVal) -> (WireVal -> T.Text) -> [Move] -> Bool
testMap reader renderer moves = 
  fromJust (fromMap (reader (renderer (asMap moves)))) == moves
