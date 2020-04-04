{-# LANGUAGE DeriveTraversable #-}

-- Functions to help me test this thing
module WorkIt where

import Strategies (Condition(..), LeafTree(..), ResultTree, toStructure, Arity(..), assess
                  , InfectionRate(..), PoolSize(..), TestLeaves, Likelihood)
import Control.Arrow ((***))
import Data.Bifoldable (biList)
import Data.List (intersperse)
import Data.Tree (Tree(..), drawTree)

import qualified Data.Map as Map

conditions :: [Condition]
conditions = cycle [Infected, Healthy]


createTree :: Int -> Int -> LeafTree Condition
createTree arity leafCount = toStructure (Arity arity) $ take leafCount conditions

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

showStats :: (Arity, InfectionRate, PoolSize, Map.Map Int Likelihood) -> String
showStats (arity, rate, pool, d) =
    unlines
    [ unwords [show arity, ",", show rate, ",", show pool]
    , unlines $ ("       | " <>) <$> unwords . biList . (show *** show) <$> Map.toList d
    ]



