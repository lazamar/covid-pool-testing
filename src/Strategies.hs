{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Strategies where

import Data.Bifunctor (first, second)
import Data.List (mapAccumL)
import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad (void)
import Control.Monad.State (State)
import Data.Tree (Tree(..))

import qualified Control.Monad.State as State

data Condition
    = Infected
    | Healthy
    deriving (Show, Eq)

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

-- The (Maybe Condition) will be Nothing if the node or leaf was not tested
type ResultTree = Tree (Maybe Condition)

data Info = Info
    { arity :: Int
    , poolSize :: Int
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
        Leaf condition -> do
            result <- runTestCmd tree =<< evaluateNode info True
            return $ Node result []

        LNode subtrees -> do
            cmd        <- evaluateNode info False
            nodeResult <- runTestCmd tree cmd
            Node nodeResult <$> traverse (evaluateTree info) subtrees

-- | Check whether the result provided is correct
isValid :: LeafTree Condition -> ResultTree -> Bool
isValid = undefined


-- | Do nothing
noop :: Monad m => a -> m ()
noop _ = return ()

-------------------------------------------------------------------------------
-- Strategies

-- A strategy to test all leaves and test no intermediary nodes.
newtype TestLeaves a = TestLeaves (Identity a)
    deriving (Eq, Show)
    deriving newtype (Applicative, Monad, Functor)

runTestLeaves :: TestLeaves a -> a
runTestLeaves (TestLeaves i) = runIdentity i

instance Strategy TestLeaves where
    evaluateNode info isLeave =
        return $ if isLeave
           then RunTest noop
           else SkipTest

-- A strategy to test only nodes at odd level numbers
newtype OddStrategy a = OddStrategy (State Int a)
    deriving newtype (Applicative, Functor, Monad, State.MonadState Int)

runOddStrategy :: OddStrategy a -> a
runOddStrategy (OddStrategy s) = State.evalState s 0

instance Strategy OddStrategy where
    evaluateNode info isLeave = do
        index <- State.get
        State.put $ index + 1
        return $ if odd index
           then RunTest noop
           else SkipTest

