{-# LANGUAGE DeriveTraversable #-}

-- Functions to help me test this thing
module WorkIt where

import Strategies (Condition(..), LeafTree(..), ResultTree, toStructure, Degree(..), assess
                  , InfectionRate(..), PoolSize(..), TestLeaves, Likelihood)
import Control.Arrow ((***))
import Data.Bifoldable (biList)
import Data.List (intersperse)
import Data.Tree (Tree(..), drawTree)

import qualified Data.Map as Map

conditions :: [Condition]
conditions = cycle [Infected, Healthy]


createTree :: Int -> Int -> LeafTree Condition
createTree arity leafCount = toStructure (Degree arity) $ take leafCount conditions


showStats :: (Degree, InfectionRate, PoolSize, Map.Map Int Likelihood) -> String
showStats (arity, rate, pool, d) =
    unlines
    [ unwords [show arity, ",", show rate, ",", show pool]
    , unlines $ tabbed <$> unwords . biList . (take 5 . (<> "   ") . show *** show) <$> Map.toList d
    , tabbed $ "Chances of using less than " <> show size <> " tests: " <>
        ( show
        $ sum
        $ fmap snd
        $ filter ((< 7) . fst)
        $ Map.toList d
        )
    ]
    where
        tabbed = ("       | " <>)
        PoolSize size = pool



