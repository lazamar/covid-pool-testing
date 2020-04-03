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

data Tree a
    = Node [Tree a]
    | Leaf a
    deriving (Foldable, Traversable, Functor, Show, Eq)

data ResultTree
    -- The (Maybe Condition) will be Nothing if the node or leaf was not tested
    = RNode (Maybe Condition) [ResultTree]
    | RLeaf (Maybe Condition)
    deriving (Show, Eq)

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
    test :: Tree Condition -> s Condition
    test tree = return $ fold tree

-- | Evaluate a tree according to a strategy
evaluateTree :: Strategy s => Info -> Tree Condition -> s ResultTree
evaluateTree info tree =
    case tree of
        Leaf condition -> do
            result <- evaluateNode info True condition
            return $ RLeaf result

        Node subtrees -> do
            nodeResult    <- evaluateNode info False =<< test tree
            childrenNodes <- traverse (evaluateTree info) subtrees
            return $ RNode nodeResult childrenNodes

-- | Check whether the result provided is correct
isValid :: Tree Condition -> ResultTree -> Bool
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
drawResult =  drawGenericTree . fmap showTest . resultToGeneric
    where
        resultToGeneric :: ResultTree -> GenericTree (Maybe Condition)
        resultToGeneric (RNode c forest) = GNode c $ fmap resultToGeneric forest
        resultToGeneric (RLeaf c       ) = GNode c []

        showTest Nothing  = "Untested"
        showTest (Just c) = "Tested " <> show c


drawTree :: Show a => Tree a -> String
drawTree =  drawGenericTree . treeToGeneric . fmap show
    where
        treeToGeneric :: Tree String -> GenericTree String
        treeToGeneric (Node forest) = GNode "" $ fmap treeToGeneric forest
        treeToGeneric (Leaf c     ) = GNode c []

drawGenericTree :: GenericTree String -> String
drawGenericTree = unlines . draw
    where
        draw :: GenericTree String -> [String]
        draw (GNode x ts0) = lines x ++ drawSubTrees ts0
          where
            drawSubTrees [] = []
            drawSubTrees [t] =
                "|" : shift "`- " "   " (draw t)
            drawSubTrees (t:ts) =
                "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

            shift first other = zipWith (++) (first : repeat other)
