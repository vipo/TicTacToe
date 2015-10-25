{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

instance Parsable Player where
    parseParam "1" = Right Player1
    parseParam "2" = Right Player2
    parseParam _ = Left "Illegal player id"

instance Parsable GameId where
    parseParam = Right . GameId

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  scottyOpts opts $ do
    get "/" $ redirect "/history"
    post "/game/:gid/player/:pid" $ do
      gameId <- param "gid"
      playerId <- param "pid"
      ct <- fmap (lookup "Content-Type") headers
      bd <- body
      (c, m, d) <- case readBoardFromWire ct bd of
                    Left l -> return l
                    Right moves -> liftIO $ record conn moves gameId playerId
      status $ Status c m
      raw d
    get "/game/:gid/player/:pid" $ do
      gameId <- param "gid"
      playerId <- param "pid"
      acc <- fmap (lookup "Accept") headers
      (c, m, d, ct) <- liftIO $ retrieveMove conn acc gameId playerId
      status $ Status c m
      setHeader "Content-Type" ct
      raw d
    get "/history/:gid" $ do
      gameId <- param "gid"
      result <- liftIO $ gameHistory conn gameId
      text result
    get "/history" $ do
      result <- liftIO $ gameTop conn
      html result
    get "/test/:id" $ do
      taskId <- param "id"
      unless (taskId >= 1 && taskId <= taskQuantity) next
      moves <- liftIO randomMoves
      extra <- liftIO randomMoves
      text $ testingModule taskId moves extra
