{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Combinatorics where

import Control.Monad (foldM)
import Data.List (permutations, mapAccumR)
import Data.Fixed (Centi, showFixed)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map

newtype Combination a = Combination { fromCombination :: [a] }
    deriving (Show, Eq)

newtype Permutation a = Permutation { fromPermutation :: [a] }
    deriving (Show, Eq)

newtype Probability = Probability { fromProbability :: Rational }
    deriving newtype (Eq, Num, Fractional, Ord)

instance Show Probability where
    show (Probability n) = "Probability " <> showAsPercentage n

showAsPercentage :: Rational -> String
showAsPercentage n = showFixed True (fromRational $ toRational $ 100 * n :: Centi) <> "%"

-- | n choose k.
binomialCoefficient :: Int -> Int -> Integer
binomialCoefficient n k = pascalsTriangle !! n !! k

-- | Memoised Pascal's Triangle
pascalsTriangle :: [[Integer]]
pascalsTriangle = iterate next [1]
    where
        next ns = zipWith (+) (0:ns) $ ns ++[0]

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

-- | How many different permutations of this list can we
-- constructo taking into account that some of its elements
-- are equal?
permutationCount :: Ord a => Combination a -> Integer
permutationCount (Combination list)
    = product
    $ snd
    $ mapAccumR
        (\remainingSpaces count ->
            ( remainingSpaces - count
            , binomialCoefficient remainingSpaces count
            )
        )
        (length list)
    $ Map.elems occurrences
    where
        occurrences = Map.fromListWith (+) $ fmap (,1) list

-- | O(nlogn)
noRepeats :: Ord a => [a] -> [a]
noRepeats = Set.toList . Set.fromList

-- | Probability of this combination, or any of its permutations
-- happening
cProbability :: Ord a => Map a Probability -> Combination a -> Probability
cProbability probabilities combination =
    Probability $ oneOccurrence * permutations
    where
        permutations = fromIntegral (permutationCount combination)

        oneOccurrence
          = fromProbability
          $ pProbability probabilities
          $ Permutation
          $ fromCombination combination

-- | Probability of this exact Permutation happening
pProbability :: Ord a => Map a Probability -> Permutation a -> Probability
pProbability probabilities permutation
    = product
    $ fmap (probabilities Map.!)
    $ fromPermutation permutation
