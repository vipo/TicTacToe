{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import Data.Aeson(FromJSON, (.:), ToJSON, (.=), object)
import Data.Scientific
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Applicative

import System.Environment(getArgs)

import Network.Wreq

import qualified Domain as D

data Mode = Attack | Defend
  deriving (Show, Eq)

instance Read Mode where
  readsPrec _ "1" = [(Attack, "")]
  readsPrec _ "2" = [(Defend, "")]
  readsPrec _ _ = []

instance ToJSON D.Move where
  toJSON (D.Move (D.Coord x) (D.Coord y) D.X) =
    object ["v" .= ("x" :: String), "x" .= x, "y" .= y]
  toJSON (D.Move (D.Coord x) (D.Coord y) D.O) =
    object ["y" .= y, "x" .= x, "v" .= ("o" :: String)]

instance FromJSON D.Move where
  parseJSON (A.Object o) =
    D.Move <$> o .: "x" <*> o .: "y" <*> o .: "v"
  parseJSON _ = empty

instance FromJSON D.Coord where
  parseJSON (A.Number n) =
    case i of
      Just v | v >= 0 && v <= 2 -> return $ D.Coord v
      _                         -> empty
    where
      i = toBoundedInteger n :: (Maybe Int)

instance FromJSON D.Value where
  parseJSON (A.String "x") = return D.X 
  parseJSON (A.String "X") = return D.X
  parseJSON (A.String "o") = return D.O
  parseJSON (A.String "O") = return D.O
  parseJSON _ = empty

urlBase :: String
urlBase = "http://tictactoe.homedir.eu"

firstMove :: D.Move
firstMove = D.Move (D.Coord 1) (D.Coord 1) D.X

main :: IO ()
main = do
  (gameId, mode, playerId) <- liftM args getArgs
  putStrLn $ concat [
    "Game id: ", gameId,
    ", mode: ", show mode,
    ", player id: ", playerId]
  when (mode == Attack) $ postBoard (gameId, playerId) [firstMove]
  foldM_ (move (gameId, playerId)) True [1 .. 4]

move :: (String, String) -> Bool -> Int -> IO Bool
move p@(gameId, playerId) continue _ =
  if not continue then return False
  else do
    board <- getBoard p
    if D.thereIsWinner board then return False
    else (postBoard p (appendMove board) >> return True)

appendMove :: [D.Move] -> [D.Move]
appendMove [] = error "First move must be already done"
appendMove moves = moves

urlForPlayer :: String -> String -> String
urlForPlayer gameId playerId =
  concat [urlBase, "/game/", gameId, "/player/", playerId]

opts :: Options
opts = defaults & header "Accept" .~ json & header "Content-Type" .~ json
  where
    json = ["application/json"]

postBoard :: (String, String) -> [D.Move] -> IO ()
postBoard (gameId, playerId) moves =
  void $ postWith opts (urlForPlayer gameId playerId) (A.toJSON moves)

getBoard :: (String, String) -> IO [D.Move]
getBoard (gameId, playerId) = do
  r <- getWith opts (urlForPlayer gameId playerId) >>= asJSON :: IO (Response [D.Move])
  return $ r ^. responseBody

args :: [String] -> (String, Mode, String)
args (g:a:[]) = (g, read a, a)
args _ = error "Illegal arguments: provide game_id and mode (1 or 2)"