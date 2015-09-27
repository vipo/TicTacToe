{-# LANGUAGE OverloadedStrings #-}

module Main where

import TicTacToe
import Web.Scotty

import Control.Monad
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as W

opts :: Options
opts = Options 1 (W.setHost "127.0.0.1" W.defaultSettings)

main :: IO ()
main = scottyOpts opts $ do
  get "/test/:id" $ do
    taskId <- param "id"
    unless (taskId >= 1 && taskId <= taskQuantity) next
    moves <- liftIO randomMoves
    extra <- liftIO randomMoves
    text $ testingModule taskId moves extra
