{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

--import Database.Redis (connect, defaultConnectInfo)

import TicTacToe
import Web.Scotty
--import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as W

opts :: Options
opts = Options 1 (W.setHost "127.0.0.1" W.defaultSettings)

instance Parsable Player where
    parseParam "1" = Right Player1
    parseParam "2" = Right Player2
    parseParam _ = Left "Illegal player id"

instance Parsable GameId where
    parseParam = Right . GameId

main :: IO ()
main =
  scottyOpts opts $
    get "/test/:id" $ do
      taskId <- param "id"
      unless (taskId >= 1 && taskId <= taskQuantity) next
      moves <- liftIO randomMoves
      text $ testingModule taskId moves
