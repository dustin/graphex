{-# LANGUAGE RankNTypes #-}

module Graphex.Search (bfsOn, bfsWith, dfsOn, dfsWith, findFirst, flood) where

import           Data.List (find)
import           Data.Set  (Set)
import qualified Data.Set  as Set

-- | A functional queue.
data Queue a = Queue [a] [a]

qpop :: Queue a -> Maybe (a, Queue a)
qpop (Queue (x:xs) r) = Just (x, if null xs then Queue (reverse r) [] else Queue xs r)
qpop _                = Nothing

qsingle :: a -> Queue a
qsingle a = Queue [a] []

qappendList :: Queue a -> [a] -> Queue  a
qappendList (Queue [] []) xs = Queue xs []
qappendList (Queue [] r) xs  = Queue (reverse r) (reverse xs)
qappendList (Queue l r) xs   = Queue l (reverse xs <> r)

type SearchFunction r a = Ord r => (a -> r) -> (a -> [a]) -> a -> [a]

-- | Find the first match using the given search function (e.g., bfsOn or dfsOn).
findFirst :: Ord r => SearchFunction r a -> (a -> r) -> (a -> [a]) -> a -> (a -> Bool) -> Maybe a
findFirst search rep nf start termination = find termination (search rep nf start)

-- | A BFS that will return a list of all reachable states.
--
-- The first argument is a representation function is used to deduplicate state.
-- For some use cases where the entire state is valid, you can use 'id'.
bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn rep next = bfsWith (\r -> Set.insert (rep r)) (\r -> Set.member (rep r)) next

-- | BFS with custom functions for remembering and recalling whether a state has been visited.
bfsWith :: Monoid s => (a -> s -> s) -> (a -> s -> Bool) -> (a -> [a]) -> a -> [a]
bfsWith remember seenf next start = go mempty (qsingle start)
  where
    go seen inq =
      case qpop inq of
        Nothing -> []
        Just (x,xs)
          | seenf x seen -> go seen xs
          | otherwise    -> x : go (remember x seen) (qappendList xs $ next x)

-- | A DFS variant of 'bfsOn'.
dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn rep next = dfsWith (\r -> Set.insert (rep r)) (\r -> Set.member (rep r)) next

-- | A DFS variant of 'bfsWith'.
dfsWith :: Monoid s => (a -> s -> s) -> (a -> s -> Bool) -> (a -> [a]) -> a -> [a]
dfsWith remember seenf next start = go mempty [start]
  where
    go _ [] = []
    go seen (x:xs)
        | seenf x seen = go seen xs
        | otherwise    = x : go (remember x seen) (next x <> xs)

-- | Flood fill a graph from a starting point and return all visited points.
flood :: Ord a => (a -> Set a) -> a -> Set a
flood nf = go mempty . Set.singleton
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map nf todo)
