{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Strategies where

import Control.Monad.State (State)
import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)
import Data.Fixed (Centi, showFixed)
import Data.Foldable (fold, asum)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (permutations,foldl')
import Data.Map (Map)
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.Tree (Tree(..), foldTree, drawTree)
import Debug.Trace

import qualified Control.Monad.State as State
import qualified Data.Set as Set
import qualified Data.Map as Map

data Condition
    = Infected
    | Healthy
    deriving (Show, Eq, Ord, Bounded, Enum)

-- | How to mix conditions together.
instance Semigroup Condition where
    Infected <> _ = Infected
    Healthy  <> c = c

instance Monoid Condition where
    mempty = Healthy
    mappend = (<>)

-- | A tree with data only in the leaves
data LeafTree a
    = Leaf a
    | LNode [LeafTree a]
    deriving (Foldable, Traversable, Functor, Show, Eq)

-------------------------------------------------------------------------------
-- Applying strategies

-- The (Maybe Condition) will be Nothing if the node or leaf was not tested
type ResultTree = Tree (Maybe Condition)

data Info = Info
    { info_arity :: Arity
    , info_poolSize :: PoolSize
    , info_infectionRate :: InfectionRate
    }
    deriving (Show, Eq)

class Monad s => Strategy s where
    -- | By being polymorphic on the response condition, we prevent
    -- this function from accidentally misdiagnosing the input.
    evaluateNode :: Info -> Bool -> s (TestCmd s)
    strategyName :: s String

data TestCmd m
    -- | This callback allows us to process the output of the test
    = RunTest (Condition -> m ())
    | SkipTest

-- | Make sure that if the test is run, the callback
-- in RunTest will be called.
runTestCmd :: Monad m => LeafTree Condition -> TestCmd m -> m (Maybe Condition)
runTestCmd tree cmd =
    case cmd of
        SkipTest  -> return Nothing
        RunTest f -> do
            let result = test tree
            f result
            return $ Just result

-- | Mix the samples of everyone in this tree and return a result
-- If anyone is infected, the result will be "Infected"
test :: LeafTree Condition -> Condition
test = fold

-- | Evaluate a tree according to a strategy
evaluateTree :: Strategy s => Info -> LeafTree Condition -> s ResultTree
evaluateTree info tree =
    case tree of
        Leaf _ -> do
            result <- runTestCmd tree =<< evaluateNode info True
            return $ Node result []

        LNode subtrees -> do
            cmd        <- evaluateNode info False
            nodeResult <- runTestCmd tree cmd
            Node nodeResult <$> traverse (evaluateTree info) subtrees

-- | Check whether the result provided is correct
--
-- The result is correct if a leaf was tested or if a leaf was
-- not tested because one of its parents tested as healthy.
isValid :: ResultTree -> Bool
isValid result = go [] result
    where
        go parents (Node subject [])     = wasTested subject || (Just Healthy == lastDiagnosed parents)
        go parents (Node mtest children) = and $ go (mtest:parents) <$> children

        lastDiagnosed :: [Maybe Condition] -> Maybe Condition
        lastDiagnosed = asum

        wasTested :: Maybe Condition -> Bool
        wasTested = isJust

-- | Do nothing
noop :: Monad m => a -> m ()
noop _ = return ()

-------------------------------------------------------------------------------
-- Generating Scenarios

newtype Arity = Arity Int
    deriving (Show, Eq)

newtype PoolSize = PoolSize Int
    deriving (Show, Eq)
-- | Likelihood of someone being infected as a number between 0 and 1
newtype InfectionRate = InfectionRate  Double
    deriving (Show, Eq)

newtype Likelihood = Likelihood Double
    deriving (Eq, Num)

-- Show as percentage cropped to two decimal points
instance Show Likelihood where
    show (Likelihood n) = showFixed True (fromRational $ toRational $ 100 * n :: Centi) <> "%"

-- | A Scenario is a certain amount of healthy and infected
-- subjects in a particular order
type Scenario = [Condition]

type Structure = LeafTree Condition

generateScenarios :: PoolSize -> [Scenario]
generateScenarios (PoolSize size) = do
    infectedCount <- [0..size]
    let healthyCount = size - infectedCount
        infected     = take infectedCount $ repeat Infected
        healthy      = take healthyCount  $ repeat Healthy
    noRepeats $ permutations (infected ++ healthy)

-- | O(nlogn)
noRepeats :: Ord a => [a] -> [a]
noRepeats = Set.toList . Set.fromList

getLikelihood :: InfectionRate -> Scenario -> Likelihood
getLikelihood (InfectionRate infectedRate) scenario =
    Likelihood $ foldr (*) 1 $ fmap toRate scenario
    where
        healthyRate = 1 - infectedRate

        toRate Healthy  = healthyRate
        toRate Infected = infectedRate

-- | Organise a scenario in a Tree of a specified arity
toStructure :: Arity -> Scenario -> Structure
toStructure (Arity arity) list = chunkIt $ fmap Leaf list
    where
        chunkIt (root:[]) = root
        chunkIt nodes = chunkIt $ fmap toNode $ chunksOf arity nodes

        toNode (root:[]) = root
        toNode nodes = LNode nodes

        chunksOf :: Int -> [a] -> [[a]]
        chunksOf _ [] = []
        chunksOf size l = take size l : chunksOf size (drop size l)

-------------------------------------------------------------------------------
-- Assessing strategy efficiency

testsUsed :: ResultTree -> Int
testsUsed = foldTree $ \node children -> toNumber node + sum children
    where
        toNumber Nothing = 0
        toNumber _       = 1

-- | Returns a map of the probability of using a certain amount of tests
-- Adds likelihoods of using the same amount of tests
probabilities :: Strategy s
    => (forall a. s a -> a) -- ^ run strategy
    -> Info
    -> [(LeafTree Condition, Likelihood)]
    -> Map Int Likelihood
probabilities run info scenarioTrees = Map.fromListWith (+) $ first eval <$> scenarioTrees
    where
        eval tree =
            let result = run (evaluateTree info tree)
            in
            if isValid result
               then testsUsed result
               else error $ unlines
                [ unwords
                    [ "The strategy"
                    , run strategyName
                    , "returned an invalid result when using configuration:"
                    , show info
                    ]
                , "Input tree:"
                , drawLeafTree tree
                , "Result tree:"
                , drawResult result
                ]

drawResult :: ResultTree -> String
drawResult = drawTree . fmap showTest
    where
        showTest Nothing  = "Untested"
        showTest (Just c) = "Tested " <> show c

drawLeafTree :: Show a => LeafTree a -> String
drawLeafTree  =  drawTree . toTree . fmap show
    where
        toTree :: LeafTree String -> Tree String
        toTree (LNode forest) = Node "" $ fmap toTree forest
        toTree (Leaf c      ) = Node c []

assess :: Strategy s
    => [Arity]
    -> [InfectionRate]
    -> [PoolSize]
    -> (forall a. s a -> a) -- ^ run the strategy
    -> [(Arity, InfectionRate, PoolSize, Map Int Likelihood)]
assess arities rates sizes run = do
    size  <- sizes
    let scenarios = generateScenarios size
    arity <- arities
    rate  <- rates
    let info = Info arity size rate
    return $
        ( arity
        , rate
        , size
        , probabilities run info $ (toStructure arity &&& getLikelihood rate) <$> scenarios
        )

-------------------------------------------------------------------------------
-- Strategies

-- A strategy to test all leaves and test no intermediary nodes.
newtype TestLeaves a = TestLeaves (Identity a)
    deriving (Eq, Show)
    deriving newtype (Applicative, Monad, Functor)

runTestLeaves :: TestLeaves a -> a
runTestLeaves (TestLeaves i) = runIdentity i

instance Strategy TestLeaves where
    strategyName = return "Test leaves"
    evaluateNode _ isLeaf =
        return $ if isLeaf
           then RunTest noop
           else SkipTest

-- A strategy to test only nodes at odd level numbers
newtype OddStrategy a = OddStrategy (State Int a)
    deriving newtype (Applicative, Functor, Monad, State.MonadState Int)

runOddStrategy :: OddStrategy a -> a
runOddStrategy (OddStrategy s) = State.evalState s 0

instance Strategy OddStrategy where
    strategyName = return "Test odd-indexed nodes"
    evaluateNode _ _ = do
        index <- State.get
        State.put $ index + 1
        return $ if odd index
           then RunTest noop
           else SkipTest

-- A strategy to test all nodes (very wasteful)
newtype TestAllNodes a = TestAllNodes (Identity a)
    deriving (Eq, Show)
    deriving newtype (Applicative, Monad, Functor)

runTestAllNodes :: TestAllNodes a -> a
runTestAllNodes  (TestAllNodes i) = runIdentity i

instance Strategy TestAllNodes  where
    strategyName = return "Test all nodes"
    evaluateNode _ _ = return $ RunTest noop

-- Only test children of infected parents
newtype TestIPChildren a = TestIPChildren (State IPInfo a)
    deriving newtype (Applicative, Functor, Monad, State.MonadState IPInfo)

type IPInfo = Map Int (Maybe Condition)

runTestIPChildren :: TestIPChildren a -> a
runTestIPChildren  (TestIPChildren s) = State.evalState s mempty

instance Strategy TestIPChildren  where
    strategyName = return "Eagerly test children of infected parents"
    evaluateNode (Info arity size rate) _ = do
        nodes <- State.get
        let index = case Map.keys nodes of
                []   -> 0
                keys -> 1 + maximum keys

            indices = indexTree arity size
            parents = mapMaybe (`Map.lookup` nodes) $ parentIndices index indices
            lastTestedParent = asum parents

        if lastTestedParent == Just Healthy
            then do
                State.put $ Map.insert index Nothing nodes
                return SkipTest
            else return $ RunTest $ \condition ->
                State.put $ Map.insert index (Just condition) nodes

-- | Returns index of parents of a node with a certain index.
-- ordered from closest parent to root
parentIndices :: Int -> IndicesTree -> [Int]
parentIndices target (IndicesTree tree) = fromMaybe [] $ go [] tree
    where
        go parents node@(Node index children) =
            if index == target
                then Just parents
                else asum $ go (index:parents) <$> children

-- | A tree holding the index of each node when traversed
-- depth-first
newtype IndicesTree = IndicesTree (Tree Int)

indexTree :: Arity -> PoolSize -> IndicesTree
indexTree arity (PoolSize size)
    = IndicesTree
    $ snd
    $ asIndices 0
    $ toStructure arity
    $ take size
    $ repeat Healthy
    where
        asIndices :: Int -> LeafTree a -> (Int, Tree Int)
        asIndices index tree =
            case tree of
                Leaf _ ->  (index, Node index [])
                LNode children ->
                    let
                        mkChild (ix, cs) child =
                            let (newIx, newChild) = asIndices (ix + 1) child
                            in
                            (newIx, newChild:cs)

                        (maxIndex, newChildren) = second reverse $ foldl' mkChild (index, []) children
                    in
                    (maxIndex, Node index newChildren)


