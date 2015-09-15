{-# LANGUAGE OverloadedStrings #-}

module Main where

import TicTacToe
import Web.Scotty

import Control.Monad
import Control.Monad.IO.Class
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.Monoid (mconcat)

import qualified Network.Wai.Handler.Warp as W

opts :: Options
opts = Options 1 (W.setHost "127.0.0.1" W.defaultSettings)

main = scottyOpts opts $ do
  get "/test/:id" $ do
    id <- param "id"
    unless (id >= 1 && id <= taskQuantity) next
    moves <- liftIO $ infinitRandomMoves
    text $ testingModule id moves
