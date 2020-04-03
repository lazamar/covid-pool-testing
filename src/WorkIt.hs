{-# LANGUAGE DeriveTraversable #-}

-- Functions to help me test this thing
module WorkIt where

import Strategies (Condition(..), LeafTree(..), ResultTree)
import Data.Tree (Tree(..), drawTree)

conditions :: [Condition]
conditions = cycle [Infected, Healthy]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size l = take size l : chunksOf size (drop size l)

createTree :: Int -> Int -> LeafTree Condition
createTree arity leafCount = chunkIt $ take leafCount $ fmap Leaf conditions
    where
        chunkIt (root:[]) = root
        chunkIt nodes = chunkIt $ fmap toNode $ chunksOf arity nodes

        toNode (root:[]) = root
        toNode nodes = LNode nodes


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

