{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module Domain
where

import Control.Lens ((.~), (&), element)

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import Data.String.Conversions

import Test.QuickCheck
import Numeric

data Action = Defence | Winner
  deriving (Show, Eq)

data Format = Bencode | Json
  deriving Show

data GameVariation = Misere | Notakto | WildTicTacToe | WildMisere
  deriving Show

data Modifier = AsIs | NoArrays | NoMaps
    deriving Show

type Task = (GameVariation, Action, Format, Modifier)
type TaskId = Int

class (Enum a, Bounded a, Eq a) => Circ a where
  next :: a -> a
  next a = if a == maxBound then minBound else succ a

data Value = X | O
  deriving (Enum, Show, Eq, Bounded)
instance Circ Value

type Moves = [Move]

instance Arbitrary Value where
    arbitrary = elements [X, O]

newtype Coord = Coord Int
    deriving (Show, Eq)
instance Arbitrary Coord where
    arbitrary = do
        v <- choose (0, 1) :: Gen Int
        return $ Coord v

newtype PlayerName = PlayerName String
  deriving (Show, Eq)

instance Arbitrary PlayerName where
  arbitrary = PlayerName <$> arbitrary

data Move = Move Coord Coord Value PlayerName
    deriving (Show, Eq)
instance Arbitrary Move where
    arbitrary = Move <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

actions :: [Action]
actions = [Defence, Winner]

formats :: [Format]
formats = [Bencode, Json]

modifiers :: [Modifier]
modifiers = [AsIs, NoArrays, NoMaps]

gameVariations :: [GameVariation]
gameVariations = [Misere, Notakto, WildTicTacToe, WildMisere]

allTasks :: [Task]
allTasks = do
  m <- modifiers
  f <- formats
  g <- gameVariations
  a <- actions
  return (g, a, f, m)

idTaskMap :: Map.Map TaskId Task
idTaskMap = Map.fromList $ zip [1 .. ] allTasks

lookupTask :: TaskId -> Maybe Task
lookupTask taskId = Map.lookup taskId idTaskMap

whitespaceNoise :: [Int] -> T.Text -> T.Text
whitespaceNoise s t = T.pack $ concatMap r (zip s (T.unpack t))
    where
        r (q, ' ') = if q > 0 then replicate q ' ' else " "
        r (_, c) = [c]

thereIsWinner :: [Move] -> Bool
thereIsWinner [] = False
thereIsWinner ms =
    case movesToVal ms of
        [a, b, c, _, _, _, _, _, _] | isJust a && a == b && b == c -> True
        [_, _, _, a, b, c, _, _, _] | isJust a && a == b && b == c -> True
        [_, _, _, _, _, _, a, b, c] | isJust a && a == b && b == c -> True
        [a, _, _, b, _, _, c, _, _] | isJust a && a == b && b == c -> True
        [_, a, _, _, b, _, _, c, _] | isJust a && a == b && b == c -> True
        [_, _, a, _, _, b, _, _, c] | isJust a && a == b && b == c -> True
        [a, _, _, _, b, _, _, _, c] | isJust a && a == b && b == c -> True
        [_, _, a, _, b, _, c, _, _] | isJust a && a == b && b == c -> True
        _ -> False
    where
        movesToVal :: [Move] -> [Maybe Value]
        movesToVal = List.foldl toVal initial
        initial = replicate 9 Nothing
        toVal acc (Move (Coord x) (Coord y) v _) = acc & element (x * 3 + y) .~ Just v


data WireVal = IntVal Int |
    StringVal T.Text |
    DictVal [(T.Text, WireVal)] |
    ListOfVals [WireVal]
    deriving (Show, Eq)

lookupInt :: T.Text -> [(T.Text, WireVal)] -> Maybe Int
lookupInt _ [] = Nothing
lookupInt key ((k, IntVal i) : xs) = if key == k then Just i else lookupInt key xs
lookupInt key (_ : xs) = lookupInt key xs

lookupString :: T.Text -> [(T.Text, WireVal)] -> Maybe T.Text
lookupString _ [] = Nothing
lookupString key ((k, StringVal s) : xs) = if key == k then Just s else lookupString key xs
lookupString key (_ : xs) = lookupString key xs

boardValue :: T.Text -> Maybe Value
boardValue "o" = Just O
boardValue "O" = Just O
boardValue "x" = Just X
boardValue "X" = Just X
boardValue _ = Nothing

asArrayOfMaps :: [Move] -> WireVal
asArrayOfMaps moves = ListOfVals $ List.map toTriple moves
    where
        toValue X = ("v", StringVal "x")
        toValue O = ("v", StringVal "o")
        toTriple (Move (Coord x) (Coord y) v (PlayerName n)) =
            DictVal [("x", IntVal x), ("y", IntVal y), toValue v, ("id", StringVal (cs n))]

fromArrayOfMaps :: WireVal -> Maybe [Move]
fromArrayOfMaps (ListOfVals vals) = mapM toMove vals
    where
        toMove :: WireVal -> Maybe Move
        toMove (DictVal pairs) = do
            x <- lookupX pairs
            y <- lookupY pairs
            v <- lookupV pairs
            n <- lookupName pairs
            return $ Move (Coord x) (Coord y) v (PlayerName (cs n))
        toMove _ = Nothing
        lookupX a = [ val | val <- lookupInt "x" a, val >= 0, val <= 2 ]
        lookupY a = [ val | val <- lookupInt "y" a, val >= 0, val <= 2 ]
        lookupV a = [ val | v <- lookupString "v" a, val <- boardValue v]
        lookupName = lookupString "id"
fromArrayOfMaps _ = Nothing

asMapOfMaps :: [Move] -> WireVal
asMapOfMaps moves = DictVal $ List.zip letters vals
    where
        ListOfVals vals = asArrayOfMaps moves
        letters = List.sort $ List.take (List.length moves) $
            List.map (\v -> T.pack (showHex v "")) ([0 .. ] :: [Integer])

fromMapOfMaps :: WireVal -> Maybe [Move]
fromMapOfMaps (DictVal pairs) = fromArrayOfMaps $ ListOfVals $ List.map snd $ List.sortOn fst pairs
fromMapOfMaps _ = Nothing

asArrayOfArrays :: [Move] -> WireVal
asArrayOfArrays moves = ListOfVals $ List.map toTriple moves
    where
        toValue X = StringVal "x"
        toValue O = StringVal "o"
        toTriple (Move (Coord x) (Coord y) v (PlayerName i)) =
            ListOfVals [StringVal "x", IntVal x, StringVal "y", IntVal y,
                StringVal "v", toValue v, StringVal "id", StringVal (cs i)]

fromArrayOfArrays :: WireVal -> Maybe [Move]
fromArrayOfArrays (ListOfVals vals) = fromArrayOfMaps $ ListOfVals $ List.map toDict vals
    where
        toDict (ListOfVals [StringVal k1, v1, StringVal k2, v2, StringVal k3, v3]) =
            DictVal [(k1, v1), (k2, v2), (k3, v3)]
        toDict _ = DictVal []
fromArrayOfArrays _ = Nothing
