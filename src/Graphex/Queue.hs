{-# LANGUAGE StrictData #-}
module Graphex.Queue where

-- | A functional queue.
data Queue a = Queue [a] [a]

qpop :: Queue a -> Maybe (a, Queue a)
qpop (Queue (x:xs) r) = Just (x, if null xs then Queue (reverse r) [] else Queue xs r)
qpop _                = Nothing

qsingle :: a -> Queue a
qsingle a = Queue [a] []

qempty :: Queue a
qempty = Queue [] []

qlist :: [a] -> Queue a
qlist xs = Queue xs []

qappendList :: Queue a -> [a] -> Queue a
qappendList (Queue [] []) xs = Queue xs []
qappendList (Queue [] r) xs  = Queue (reverse r) (reverse xs)
qappendList (Queue l r) xs   = Queue l (reverse xs <> r)
