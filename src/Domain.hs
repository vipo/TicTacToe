module Domain
where

import qualified Data.Map as Map
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
    deriving Show
instance Arbitrary Coord where
    arbitrary = do
        v <- choose (0, 1) :: Gen Int
        return $ Coord v

data Move = Move Coord Coord Value
    deriving Show
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
