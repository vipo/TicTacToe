{-# LANGUAGE OverloadedStrings #-}

module Main where

import TicTacToe
import Web.Scotty

import Control.Monad

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/test/:id" $ do
    id <- param "id"
    unless (id >= 1 && id <= taskQuantity) next 
    text $ testingModule id
