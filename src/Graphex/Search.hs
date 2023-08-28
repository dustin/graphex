module Graphex.Search (bfsOn, flood) where

import           Data.Set (Set)
import qualified Data.Set as Set

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

-- | A BFS that will return a list of all reachable states.
--
-- The first argument is a representation function is used to deduplicate state.
-- For some use cases where the entire state is valid, you can use 'id'.
bfsOn ::  Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = go Set.empty (qsingle start)
  where
    go seen inq =
      case qpop inq of
        Nothing -> []
        Just (x,xs)
          | Set.member r seen ->     go seen xs
          | otherwise         -> x : go (Set.insert r seen) (qappendList xs $ next x)
          where r = rep x

-- | Flood fill a graph from a starting point.
flood :: Ord a => (a -> Set a) -> a -> Set a
flood nf = go mempty . Set.singleton
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map nf todo)
