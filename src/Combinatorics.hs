module Combinatorics where

import Control.Monad (foldM)
import Data.List (permutations)

import qualified Data.Set as Set


newtype Combination a = Combination { fromCombination :: [a] }
    deriving (Show, Eq)

newtype Permutation a = Permutation { fromPermutation :: [a] }
    deriving (Show, Eq)

binomialCoefficient :: Int -> Int -> Double
binomialCoefficient n k =
    fromIntegral (factorial n) / fromIntegral (factorial k * factorial (n - k))

factorial :: Int -> Int
factorial n = allFactorials !! n

-- | Memoised list of all existing factorials
allFactorials :: [Int]
allFactorials = 1 : zipWith (*) [1..] allFactorials

-- | Slow
allCombinations :: Ord a => [a] -> Int -> [Combination a]
allCombinations elements targetLength =
    fmap Combination $ foldM addPossibilities [] $ uniques
    where
        uniques = noRepeats elements
        lastEl  = last uniques

        -- | This is O(n^2). If there are performance problems use
        -- difference lists and pass the new length forward
        addPossibilities xs x = do
            let maxCount = targetLength - length xs
            count <- if x == lastEl
                        then [   maxCount]
                        else [0..maxCount]
            return $ take count (repeat x) ++ xs

-- | Very slow
allPermutations :: Ord a => Combination a -> [Permutation a]
allPermutations (Combination list) =
    fmap Permutation $ noRepeats $ permutations list


-- | O(nlogn)
noRepeats :: Ord a => [a] -> [a]
noRepeats = Set.toList . Set.fromList
