module Domain
where

import qualified Data.Map as Map

data Action = Validate | Defence | Winner
    deriving Show

data Format = Bencode | Json | SExpr | MExpr | Scala
    deriving Show

data Modifier = AsIs | NoArrays
    deriving Show

type Task = (Action, Format, Modifier)
type TaskId = Int

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
