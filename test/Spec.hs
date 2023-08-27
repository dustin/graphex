import           Control.Monad         (foldM, guard)
import           Data.Foldable         (fold)
import           Data.Map              (Map)
import qualified Data.Map.Strict       as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           GHC.Generics          (Generic)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Graphex

-- It's usually a terrible idea to write your own Show instance, but this is just for debugging.
instance Show Graph where
    show (Graph m) = fold [show k <> " -> " <> show (Set.toList vs) <> ", " | (k, vs) <- Map.assocs m]

-- I asked ChatGPT to make me a list of arbitrary strings in Haskell format.
someStrings :: [Text]
someStrings =
    ["x", "y", "z", "foo", "bar", "baz", "qux", "hello", "world",
     "alpha", "beta", "gamma", "delta", "list", "value", "result",
     "input", "output", "function", "variable"]

instance Arbitrary Graph where
    arbitrary = do
        -- We begin with some subset of keys in arbitrary order
        somekeys <- shuffle =<< sublistOf someStrings
        -- We build a graph of acyclic connections, ensuring that all keys are present
        Graph . (<> Map.fromList [(k, mempty) | k <- somekeys]) <$> foldM next mempty somekeys
        where
            next m k = flip (Map.insert k) m . Set.fromList <$> sublistOf (Map.keys m)

    -- A custom shrink here needs to make sure
    shrink (Graph m) = Graph . Map.fromList <$> keySubs (Map.assocs m)
        where
            keySubs = fmap clean . shrinkList (const [])
            clean kvs = (\(k, vs) -> (k, vs `Set.intersection` keys)) <$> kvs
                where
                    keys = Set.fromList (fmap fst kvs)

-- A DAG that we know contains the given node.
data GraphWithKey = GraphWithKey Text Graph
    deriving stock (Show, Eq, Generic)

instance Arbitrary GraphWithKey where
    arbitrary = do
        Graph g <- arbitrary `suchThat` (\(Graph m) -> (not . Map.null) m)
        k <- QC.elements (Map.keys g)
        pure $ GraphWithKey k (Graph g)

    -- When shrinking, our k is constant and should still be in the resulting map keyset (though links can be removed)
    shrink (GraphWithKey k g) = GraphWithKey k <$> filter (\(Graph m) -> Map.member k m) (shrink g)

-- A DAG that we know contains two keys that are connected (a path exists to the second one from the first one).
data ConnectedGraph = ConnectedGraph Text Text Graph
    deriving stock (Show, Eq, Generic)

instance Arbitrary ConnectedGraph where
    arbitrary = do
        (g@(Graph m), connMap) <- arbitrary `suchThatMap` connections
        from <- QC.elements (Map.keys connMap)
        to <- QC.elements (connMap Map.! from)
        pure $ ConnectedGraph from to g
        where
            connections :: Graph -> Maybe (Graph, Map Text [Text])
            connections g@(Graph m) = do
                let connMap = Map.fromListWith (<>) [(k, [ks]) | k <- Map.keys m, ks <- Set.toList (Set.delete k $ allDepsOn g k)]
                guard (not . Map.null $ connMap)
                pure (g, connMap)

    -- When shrinking, our k is constant and should still be in the resulting map keyset (though links can be removed)
    shrink (ConnectedGraph from to g) = ConnectedGraph from to <$> filter ((\(Graph m) -> Map.member from m && Map.member to m)) (shrink g)

prop_reverseEdgesId :: Graph -> Property
prop_reverseEdgesId g =
    counterexample ("fwd: " <> show g <> "\n rev: " <> show rg) $
    g === reverseEdges (reverseEdges g)
    where
        rg = reverseEdges g

prop_reversedDep :: Graph -> Property
prop_reversedDep g@(Graph m) =
    counterexample ("fwd: " <> show g <> "\nrev: " <> show rg) $
    all (\(k, vs) -> all (\k' -> k `elem` directDepsOn rg k') vs) (Map.assocs m)
    where
        rg = reverseEdges g

prop_reachableIsFindable :: ConnectedGraph -> Bool
prop_reachableIsFindable gwk@(ConnectedGraph from to g) = not . null $ why g from to

prop_negativePathfinding :: Graph  -> Property
prop_negativePathfinding g = forAll oneGoodKey (null . uncurry (why g))
    where oneGoodKey = do
            good <- elements someStrings
            let bad = "missingnode"
            elements [ (good, bad), (bad, good) ]

prop_notSelfDepDirect :: GraphWithKey -> Bool
prop_notSelfDepDirect (GraphWithKey k g) = k `notElem` directDepsOn g k

prop_allDepsContainsSelfDeps :: GraphWithKey -> Bool
prop_allDepsContainsSelfDeps (GraphWithKey k g) = directDepsOn g k `Set.isSubsetOf` allDepsOn g k

prop_ranking :: GraphWithKey -> Bool
prop_ranking (GraphWithKey k g) = length (allDepsOn g k) == rankings g Map.! k

prop_restrictedGraphHasSameDeps :: GraphWithKey -> Bool
prop_restrictedGraphHasSameDeps (GraphWithKey k g) = allDepsOn g k == allDepsOn (restrictTo g (allDepsOn g k)) k

prop_restrictedNodesShouldBeDeps :: GraphWithKey -> Property
prop_restrictedNodesShouldBeDeps (GraphWithKey k g) =
  let restricted = restrictTo g (allDepsOn g k)
      edges = Map.toList (unGraph restricted)
      validNodes = Set.insert k $ allDepsOn g k
  in counterexample ("restricted: " <> show restricted) $
     all (\(k, vs) -> Set.member k validNodes && all (flip Set.member validNodes) vs) edges

prop_allPathsShouldIncludeShortest :: ConnectedGraph -> Property
prop_allPathsShouldIncludeShortest gwk@(ConnectedGraph from to g) =
    why g from to === why (allPathsTo g from to) from to

prop_allPaths :: ConnectedGraph -> Property
prop_allPaths gwk@(ConnectedGraph from to g) =
    counterexample ("allPaths: " <> show allPaths <> "\nallFrom: " <> show allFrom <> "\nallTo: " <> show allTo <> "\ninBoth: " <> show inBoth) $
    conjoin ((\(k, (f,t)) -> counterexample ("  at " <> show k <> ": " <> show (f,t)) $ f .&&. t) <$> got)
    where
        got = mapMaybeWithKey (\k -> if k `elem` [from, to] then Nothing else Just (validated k)) g
        validated k = (checkPath (k `Set.member` inBoth) from k, checkPath (k `Set.member` inBoth) k to)
        allFrom = allDepsOn g from
        allTo = allDepsOn (reverseEdges g) to
        inBoth = allFrom `Set.intersection` allTo
        allPaths = allPathsTo g from to
        hasPath f = not . null . why allPaths f
        checkPath True f  = hasPath f
        checkPath False f = not . hasPath f

prop_importExport :: Graph -> Property
prop_importExport g =
    counterexample ("exported: " <> show exported <> "\nimported: " <> show imported) $
    imported === g
    where
        exported = graphToDep g
        imported = depToGraph exported

tests :: [TestTree]
tests = [
    testProperty "double edge reverse is id" prop_reverseEdgesId,
    testProperty "reversed dep is still dep" prop_reversedDep,
    testProperty "we can find a path between any two reachable nodes" prop_reachableIsFindable,
    testProperty "we can't find a path when there isn't one" prop_negativePathfinding,
    testProperty "direct deps doesn't include self" prop_notSelfDepDirect,
    testProperty "all deps contains direct deps" prop_allDepsContainsSelfDeps,
    testProperty "all paths include shortest path" prop_allPathsShouldIncludeShortest,
    testProperty "all paths seems right" prop_allPaths,
    testProperty "ranking is the same size as all deps" prop_ranking,
    testProperty "restricted input has the same deps as the original" prop_restrictedGraphHasSameDeps,
    testProperty "restricted input should only contain nodes that are deps of the original" prop_restrictedNodesShouldBeDeps,
    testProperty "can round trip import/export of graph" prop_importExport
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
