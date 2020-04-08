{-# LANGUAGE RankNTypes #-}

module Main where

import Combinatorics
import Strategies
import WorkIt

import Data.Fixed (Centi, showFixed)
import Data.Map (Map)
import Data.List (intersperse, sortBy)
import System.IO (writeFile)

import qualified Data.Map as Map

main :: IO ()
main =
    writeFile "plots/stats-0.csv"
        $ toCsv
        $ fmap (populationStats population)
        $ complex
    where
        pools   = PoolSize <$> [3]
        degrees = Degree   <$> [3]
        rates   = InfectionRate <$> [0.2]

        -- | A number divisible by all pool sizes
        --population = Population $ foldr lcm 1 $ fromPoolSize <$> pools
        population = Population 100

        simple  = assessOneLevel pools rates runTestIPChildren
        complex = assess degrees rates pools runTestIPChildren


showStats' res@(Degree degree,_,_,prob) =
    unlines
        [ showStats res
        , unlines $ show <$> ap
        , "Total :" <> show (sum (fmap snd ap))
        ]

    where
        peopleToTest = 100
        testBuckets  = peopleToTest `div` degree
        ap = Map.toList (sequentialApplications testBuckets prob)

type OneLevelStats = (Degree, InfectionRate, PoolSize, Map Int Probability)

populationStats :: Population -> OneLevelStats -> Row
populationStats (Population population) (Degree degree, rate, size, probs) =
    ( Population population
    , rate
    , size
    , Degree degree
    , sequentialApplications buckets probs
    )
    where
        buckets = population `div` degree


newtype Population = Population Int


type Row = (Population, InfectionRate, PoolSize, Degree, Map Int Probability)

toCsv :: [Row] -> String
toCsv rows
  = unlines
    $ fmap (concat . intersperse ",")
    $ (:)
        [ "Population"
        , "InfectionRate"
        , "GroupSize" -- PoolSize
        , "TreeDegree"
        , "TestsUsed"
        , "Probability"
        ]
    $ foldMap toRow
    $ sortBy (\(_,r1,_,_,_) (_,r2,_,_,_) -> compare r1 r2 )
    $ sortBy (\(_,_,s1,_,_) (_,_,s2,_,_) -> compare s1 s2 )
    $ sortBy (\(_,_,_,d1,_) (_,_,_,d2,_) -> compare d1 d2 )
    $ rows

    where
        toRow (Population population, InfectionRate rate, PoolSize size, Degree degree, probs) = do
            (testsUsed, Probability probability) <- Map.toList probs
            return
                [ show population
                , showTwoDecimalPlaces rate
                , show size
                , show degree
                , show testsUsed
                , showTwoDecimalPlaces probability
                ]

showTwoDecimalPlaces n = showFixed True (fromRational $ toRational n :: Centi)

probabilityOfUsingLessThanNTests :: Int -> Map Int Probability -> Probability
probabilityOfUsingLessThanNTests n probs
    = sum
    $ map snd
    $ filter ((< n) . fst)
    $ Map.toList probs

