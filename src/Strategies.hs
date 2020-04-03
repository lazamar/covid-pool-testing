{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Strategies where

import Data.Bifunctor (first, second)
import Data.List (mapAccumL)
import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.State (State)
import Data.Tree (Tree(..), drawTree)

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

newtype Arity = Arity Int
    deriving (Show, Eq)

class Monad s => Strategy s where
    -- | By being polymorphic on the response condition, we prevent
    -- this function from accidentally misdiagnosing the input.
    evaluateNode :: Info -> Bool -> condition -> s (Maybe condition)
    -- | Mix the samples of everyone in this tree and return a result
    -- If anyone is infected, the result will be "Infected"
    test :: LeafTree Condition -> s Condition
    test tree = return $ fold tree

-- | Evaluate a tree according to a strategy
evaluateTree :: Strategy s => Info -> LeafTree Condition -> s ResultTree
evaluateTree info tree =
    case tree of
        Leaf condition -> do
            result <- evaluateNode info True condition
            return $ Node result []

        LNode subtrees -> do
            nodeResult    <- evaluateNode info False =<< test tree
            childrenNodes <- traverse (evaluateTree info) subtrees
            return $ Node nodeResult childrenNodes

-- | Check whether the result provided is correct
isValid :: LeafTree Condition -> ResultTree -> Bool
isValid = undefined


-------------------------------------------------------------------------------
-- Strategy to test all leaves and test no intermediary nodes.

newtype TestLeaves a = TestLeaves (Identity a)
    deriving (Eq, Show)
    deriving newtype (Applicative, Monad, Functor)

runTestLeaves :: TestLeaves a -> a
runTestLeaves (TestLeaves i) = runIdentity i

instance Strategy TestLeaves where
    evaluateNode info isLeave condition =
        if isLeave
           then return (Just condition)
           else return Nothing
    test tree = return $ fold tree

-------------------------------------------------------------------------------
-- Strategy to test only nodes at odd level numbers

newtype OddStrategy a = OddStrategy (State Int a)
    deriving newtype (Applicative, Functor, Monad, State.MonadState Int)

runOddStrategy :: OddStrategy a -> a
runOddStrategy (OddStrategy s) = State.evalState s 0

instance Strategy OddStrategy where
    evaluateNode info isLeave condition = do
        index <- State.get
        State.put $ index + 1
        if odd index
           then return (Just condition)
           else return Nothing
    test tree = return $ fold tree


-------------------------------------------------------------------------------

data GenericTree a = GNode a [GenericTree a]
    deriving (Functor)

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

