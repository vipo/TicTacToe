{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}

module Domain
where

import           Control.Lens            (element, (&), (.~))

import qualified Data.List               as List
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Ord                as Ord
import qualified Data.Text.Lazy          as T

import           Control.Arrow
import           Data.String.Conversions

import           Numeric
import           Test.QuickCheck

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
  arbitrary = PlayerName <$> genSafeName
    where
      genSafeName :: Gen String
      genSafeName =
          let
            gen = elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
            in listOf gen

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
        r (_, c)   = [c]

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
        _                           -> False
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

lookupPair :: T.Text -> [(T.Text, WireVal)] -> Maybe (Int, Int)
lookupPair k a =
  case List.lookup k a of
    Just (ListOfVals [IntVal v1, IntVal v2]) -> Just (v1, v2)
    _                                        -> Nothing

lookupDict :: T.Text -> [(T.Text, WireVal)] -> Maybe (Int, Int)
lookupDict k a =
  case List.lookup k a of
    Just (DictVal ts) -> extr $ List.sortBy (Ord.comparing fst) ts
    _                 -> Nothing
  where
    extr :: [(T.Text, WireVal)] -> Maybe (Int, Int)
    extr [(_, IntVal v1), (_, IntVal v2)] = Just (v1, v2)
    extr _                                = Nothing

boardValue :: T.Text -> Maybe Value
boardValue "o" = Just O
boardValue "O" = Just O
boardValue "x" = Just X
boardValue "X" = Just X
boardValue _   = Nothing

asArrayOfMaps :: [Move] -> WireVal
asArrayOfMaps [] = DictVal []
asArrayOfMaps moves =
  let
    toValue X = ("v", StringVal "x")
    toValue O = ("v", StringVal "o")
    toDict (Move (Coord x) (Coord y) v (PlayerName n)) prev =
      let
        p = case prev of
          Nothing -> []
          Just va -> [("prev", va)]
        in DictVal $ [("c", ListOfVals [IntVal x, IntVal y]), toValue v, ("id", StringVal (cs n))] ++ p
    appendPrev d m = toDict m (Just d)
    (la : re) = reverse moves
    in foldl appendPrev (toDict la Nothing) re

toInits :: WireVal -> [WireVal]
toInits g@(DictVal _) = coll g []
  where
    coll d@(DictVal vals) acc =
      case List.lookup "prev" vals of
        Nothing -> d : acc
        Just v  -> coll v (d : acc)
    coll _ acc = acc
toInits _ = []

fromArrayOfMaps :: WireVal -> Maybe [Move]
fromArrayOfMaps (DictVal []) = Just []
fromArrayOfMaps d@(DictVal _) = fmap reverse $ mapM toMove $ toInits d
    where
        toMove :: WireVal -> Maybe Move
        toMove (DictVal pairs) = do
            v <- lookupV pairs
            c <- lookupC pairs
            n <- lookupName pairs
            return $ Move (Coord (fst c)) (Coord (snd c)) v (PlayerName (cs n))
        toMove _ = Nothing
        lookupV a = [ val | v <- lookupString "v" a, val <- boardValue v]
        lookupC a = [ (c1, c2) | (c1, c2) <- lookupPair "c" a, c1 >= 0, c1 <= 2, c2 >= 0, c2 <= 2]
        lookupName = lookupString "id"
fromArrayOfMaps _ = Nothing

asMapOfMaps :: [Move] -> WireVal
asMapOfMaps moves = asMap $ asArrayOfMaps moves
    where
        letters = List.map (\v -> T.pack (showHex v "")) ([0 .. ] :: [Integer])
        asMap :: WireVal -> WireVal
        asMap (ListOfVals v) = DictVal $ List.zip (List.sort $ List.take (length v) letters) $ map asMap v
        asMap (DictVal d)    = DictVal $ map (Control.Arrow.second asMap) d
        asMap v              = v

fromMapOfMaps :: WireVal -> Maybe [Move]
fromMapOfMaps (DictVal []) = Just []
fromMapOfMaps d@(DictVal _) = fmap reverse $ mapM toMove $ toInits d
    where
        toMove :: WireVal -> Maybe Move
        toMove (DictVal pairs) = do
            v <- lookupV pairs
            c <- lookupC pairs
            n <- lookupName pairs
            return $ Move (Coord (fst c)) (Coord (snd c)) v (PlayerName (cs n))
        toMove _ = Nothing
        lookupV a = [ val | v <- lookupString "v" a, val <- boardValue v]
        lookupC a = [ (c1, c2) | (c1, c2) <- lookupDict "c" a, c1 >= 0, c1 <= 2, c2 >= 0, c2 <= 2]
        lookupName = lookupString "id"
fromMapOfMaps _ = Nothing

asArrayOfArrays :: [Move] -> WireVal
asArrayOfArrays moves = asArr $ asArrayOfMaps moves
    where
        asArr :: WireVal -> WireVal
        asArr (ListOfVals v) = ListOfVals $ map asArr v
        asArr (DictVal d) = ListOfVals $ d >>= (\(k, v) -> [StringVal k, asArr v])
        asArr v = v

fromArrayOfArrays :: WireVal -> Maybe [Move]
fromArrayOfArrays (ListOfVals []) = Just []
fromArrayOfArrays l@(ListOfVals _) = fromArrayOfMaps $ toDict l
  where
    toDict (ListOfVals [StringVal k1, v1, StringVal k2, v2, StringVal k3, v3]) =
        DictVal [(k1, v1), (k2, v2), (k3, v3)]
    toDict (ListOfVals [StringVal k1, v1, StringVal k2, v2, StringVal k3, v3, StringVal k4, v4]) =
        case List.partition (\(k,_) -> k == "prev") [(k1, v1), (k2, v2), (k3, v3), (k4, v4)] of
          ([(k, v)], ts) -> DictVal $ (k, toDict v) : ts
          _ -> DictVal []
    toDict _ = DictVal []
fromArrayOfArrays _ = Nothing
