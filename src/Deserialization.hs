{-# LANGUAGE OverloadedStrings #-}

module Deserialization

where

import Domain

import Data.Text.Lazy as T
import qualified Data.List as L

readBencode :: Text -> WireVal
readBencode _ = undefined

readJson :: Text -> WireVal
readJson _ = undefined

readMExpr :: Text -> WireVal
readMExpr _ = undefined

readSExpr :: Text -> WireVal
readSExpr _ = undefined

readScala :: Text -> WireVal
readScala _ = undefined
