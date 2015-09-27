{-# LANGUAGE OverloadedStrings #-}

module Deserialization

where

import Domain

import qualified Data.Text.Lazy as T

readBencode :: T.Text -> WireVal
readBencode _ = undefined

readJson :: T.Text -> WireVal
readJson _ = undefined

readMExpr :: T.Text -> WireVal
readMExpr _ = undefined

readSExpr :: T.Text -> WireVal
readSExpr _ = undefined

readScala :: T.Text -> WireVal
readScala _ = undefined
