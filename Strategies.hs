{-# LANGUAGE DeriveTraversable #-}

module Strategies where

import Data.Bifunctor (first, second)
import Data.List (mapAccumL)
import Data.Monoid ((<>))
import Data.Foldable (fold)

main =
    putStrLn "Hello"

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


data EvaluationMode
    = Sequential
    | Parallel

data Info = Info
    { arity :: Int
    , poolSize :: Int
    }

newtype Arity = Arity Int

class Monad s => Strategy s where
    siblingEvaluation :: s EvaluationMode
    -- | By being polymorphic on the response condition, we prevent
    -- this function from accidentally misdiagnosing the input.
    evaluateNode :: Info -> condition -> s (Maybe condition)
    -- | Mix the samples of everyone in this tree and return a result
    -- If anyone is infected, the result will be "Infected"
    test :: Tree Condition -> s Condition
    test tree = return $ fold tree


-- | Evaluate a tree according to a strategy
evaluateTree :: Strategy s => Info -> Tree Condition -> s ResultTree
evaluateTree info tree =
    case tree of
        Leaf condition -> do
            result <- evaluateNode info condition
            return $ RLeaf result

        Node subtrees -> do
            nodeResult    <- evaluateNode info =<< test tree
            mode          <- siblingEvaluation
            childrenNodes <- evaluateLevel mode (evaluateTree info) subtrees
            return $ RNode nodeResult childrenNodes

evaluateLevel :: (Applicative m, Monad m) => EvaluationMode -> (input -> m output) -> [input] -> m [output]
evaluateLevel mode f list =
    case mode of
      Sequential -> traverse f list
      Parallel   -> sequence $ fmap f list

-- | Check whether the result provided is correct
isValid :: Tree Condition -> ResultTree -> Bool
isValid = undefined

