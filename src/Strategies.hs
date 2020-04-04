{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Strategies where

import Control.Monad.State (State)
import Data.Foldable (fold, asum)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (permutations)
import Data.Maybe (isJust)
import Data.Tree (Tree(..))

import qualified Control.Monad.State as State
import qualified Data.Set as Set

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
    , info_poolSize :: Int
    }
    deriving (Show, Eq)

class Monad s => Strategy s where
    -- | By being polymorphic on the response condition, we prevent
    -- this function from accidentally misdiagnosing the input.
    evaluateNode :: Info -> Bool -> s (TestCmd s)

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
-- Assessing strategy efficiency

newtype Arity = Arity Int
    deriving (Show, Eq)

-- | Likelihood of someone being infected as a number between 0 and 1
newtype InfectionRate = InfectionRate  Double
    deriving (Show, Eq)

newtype Likelihood = Likelihood Double
    deriving (Show, Eq)

-- | A Scenario is a certain amount of healthy and infected
-- subjects in a particular order
type Scenario = [Condition]

generateTrees :: InfectionRate -> Int -> Arity -> [(Likelihood, LeafTree Condition)]
generateTrees rate sampleSize arity =
    fmap
        (\s -> (getLikelihood rate s, toLeafTree arity s))
        (generateScenarios sampleSize)

generateScenarios :: Int -> [Scenario]
generateScenarios sampleSize = do
    infectedCount <- [0..sampleSize]
    let healthyCount = sampleSize - infectedCount
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

toLeafTree :: Arity -> [a] -> LeafTree a
toLeafTree (Arity arity) list = chunkIt $ fmap Leaf list
    where
        chunkIt (root:[]) = root
        chunkIt nodes = chunkIt $ fmap toNode $ chunksOf arity nodes

        toNode (root:[]) = root
        toNode nodes = LNode nodes

        chunksOf :: Int -> [a] -> [[a]]
        chunksOf _ [] = []
        chunksOf size l = take size l : chunksOf size (drop size l)


-------------------------------------------------------------------------------
-- Strategies

-- A strategy to test all leaves and test no intermediary nodes.
newtype TestLeaves a = TestLeaves (Identity a)
    deriving (Eq, Show)
    deriving newtype (Applicative, Monad, Functor)

runTestLeaves :: TestLeaves a -> a
runTestLeaves (TestLeaves i) = runIdentity i

instance Strategy TestLeaves where
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
    evaluateNode _ _ = do
        index <- State.get
        State.put $ index + 1
        return $ if odd index
           then RunTest noop
           else SkipTest

