{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module Domain
where

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck
import Numeric

data Action = Validate | Defence | Winner
    deriving (Show, Eq)

data Format = Bencode | Json | SExpr | MExpr | Scala
    deriving Show

data Modifier = AsIs | NoArrays
    deriving Show

type Task = (Action, Format, Modifier)
type TaskId = Int

data Value = X | O
    deriving (Show, Eq)

instance Arbitrary Value where
    arbitrary = elements [X, O]

data Coord = Coord Int
    deriving (Show, Eq)
instance Arbitrary Coord where
    arbitrary = do
        v <- choose (0, 1) :: Gen Int
        return $ Coord v

data Move = Move Coord Coord Value
    deriving (Show, Eq)
instance Arbitrary Move where
    arbitrary = Move <$> arbitrary <*> arbitrary <*> arbitrary

actions :: [Action]
actions = [Validate, Defence, Winner]

formats :: [Format]
formats = [Bencode, Json, SExpr, MExpr, Scala]

modifiers :: [Modifier]
modifiers = [AsIs, NoArrays]

allTasks :: [Task]
allTasks = do
    m <- modifiers
    f <- formats
    a <- actions
    return (a, f, m)

idTaskMap :: Map.Map TaskId Task
idTaskMap = Map.fromList $ zip [1 .. ] allTasks

lookupTask :: TaskId -> Maybe Task
lookupTask taskId = Map.lookup taskId idTaskMap

data WireVal = IntVal Int |
    StringVal T.Text |
    DictVal [(T.Text, WireVal)] |
    ListOfVals [WireVal]
    deriving (Show, Eq)

asArray :: [Move] -> WireVal
asArray moves = ListOfVals $ List.map toTriple moves
    where
        toValue X = ("v", StringVal "x")
        toValue O = ("v", StringVal "o")
        toTriple (Move (Coord x) (Coord y) v) =
            DictVal [("x", IntVal x), ("y", IntVal y), toValue v]

fromArray :: WireVal -> Maybe [Move]
fromArray (ListOfVals vals) = sequence $ map toMove vals
    where
        toMove :: WireVal -> Maybe Move
        toMove (DictVal pairs) = do
            x <- lookupX pairs
            y <- lookupY pairs
            v <- lookupV pairs
            return $ Move (Coord x) (Coord y) v
        toMove _ = Nothing
        lookupX a = [ val | val <- lookupInt "x" a, val >= 0, val <= 2 ]
        lookupY a = [ val | val <- lookupInt "y" a, val >= 0, val <= 2 ]
        lookupV a = [ val | v <- lookupString "v" a, val <- boardValue v]
fromArray _ = Nothing

lookupInt :: T.Text -> [(T.Text, WireVal)] -> Maybe Int
lookupInt _ [] = Nothing
lookupInt key ((k, IntVal i) : xs) = if key == k then (Just i) else lookupInt key xs
lookupInt key (_ : xs) = lookupInt key xs

lookupString :: T.Text -> [(T.Text, WireVal)] -> Maybe T.Text
lookupString _ [] = Nothing
lookupString key ((k, StringVal s) : xs) = if key == k then (Just s) else lookupString key xs
lookupString key (_ : xs) = lookupString key xs

boardValue :: T.Text -> Maybe Value
boardValue "o" = Just O
boardValue "O" = Just O
boardValue "x" = Just X
boardValue "X" = Just X
boardValue _ = Nothing

asMap :: [Move] -> WireVal
asMap moves = DictVal $ List.zip letters vals
    where
        ListOfVals vals = asArray moves
        letters = List.sort $ List.take (List.length moves) $
            List.map (\v -> T.pack (showHex v "")) ([0 .. ] :: [Integer])

fromMap :: WireVal -> Maybe [Move]
fromMap (DictVal pairs) = fromArray $ ListOfVals $ List.map snd $ List.sortOn fst pairs
fromMap _ = Nothing
