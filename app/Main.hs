{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Redis (connect, defaultConnectInfo)

import TicTacToe
import Web.Scotty
import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as W

opts :: Options
opts = Options 1 (W.setHost "127.0.0.1" W.defaultSettings)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  scottyOpts opts $ do
    post "/game/:id" $ do
      gameId <- param "id"
      ct <- fmap (lookup "Content-Type") headers
      bd <- body
      (c, m, d) <-  case readBoardFromWire ct bd of
                    Left l -> return l
                    Right moves -> liftIO $ record conn moves gameId
      status $ Status c m
      raw d
    get "/test/:id" $ do
      taskId <- param "id"
      unless (taskId >= 1 && taskId <= taskQuantity) next
      moves <- liftIO randomMoves
      extra <- liftIO randomMoves
      text $ testingModule taskId moves extra
