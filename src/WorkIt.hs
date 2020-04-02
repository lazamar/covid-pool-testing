-- Functions to help me test this thing
module WorkIt where

import Strategies

conditions = cycle [Infected, Healthy]

chunksOf :: Int -> [a] -> [[a]]
chunksOf size [] = []
chunksOf size l = take size l : chunksOf size (drop size l)

createTree :: Int -> Int -> Tree Condition
createTree arity leafCount = chunkIt $ take leafCount $ fmap Leaf conditions
    where
        chunkIt (root:[]) = root
        chunkIt nodes = chunkIt $ fmap Node $ chunksOf arity nodes

