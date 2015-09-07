{-# LANGUAGE OverloadedStrings #-}

module TicTacToe
where

import Data.Text.Lazy as T
import Data.List as L

import Domain

taskQuantity :: Int
taskQuantity = L.length allTasks

testingModule :: TaskId -> Text
testingModule id = renderTask $ lookupTask id

renderTask :: Maybe Task -> Text
renderTask Nothing = ""
renderTask (Just (action, format, modifier)) =
    let moduleName = "module TicTacToe." ++ show format ++ "\nwhere\n\n"
        dataSignature = "message :: String\n"
        dataFunction = "message = \"dasdasd\""
    in T.pack $ moduleName ++ dataSignature ++ dataFunction ++ "\n"
