{-# LANGUAGE OverloadedStrings #-}

module Domain
where

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Applicative
import Test.QuickCheck

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
lookupTask id = Map.lookup id idTaskMap 

data WireVal = IntVal Int |
    StringVal T.Text |
    DictVal [(T.Text, WireVal)] |
    ListOfVals [WireVal]
    deriving Show

asArray :: [Move] -> WireVal
asArray moves = ListOfVals $ List.map toTriple moves
    where
        toValue X = ("v", StringVal "x")
        toValue O = ("v", StringVal "o")
        toTriple (Move (Coord x) (Coord y) v) =
            DictVal [("x", IntVal x), ("y", IntVal y), toValue v]

fromArray :: WireVal -> Maybe [Move]
fromArray _ = Nothing

asMap :: [Move] -> WireVal
asMap moves = DictVal $ List.zip letters vals
    where
        ListOfVals vals = asArray moves
        letters = List.map (T.pack . (:[])) ['a' .. 'z']

fromMap :: WireVal -> Maybe [Move]
fromMap _ = Nothing
