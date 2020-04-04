{-# LANGUAGE DeriveTraversable #-}

-- Functions to help me test this thing
module WorkIt where

import Strategies (Condition(..), LeafTree(..), ResultTree, toLeafTree, Arity(..))
import Data.Tree (Tree(..), drawTree)

conditions :: [Condition]
conditions = cycle [Infected, Healthy]


createTree :: Int -> Int -> LeafTree Condition
createTree arity leafCount = toLeafTree (Arity arity) $ take leafCount conditions

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

