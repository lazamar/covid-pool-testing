{-# LANGUAGE RankNTypes #-}

module Main where

import Combinatorics
import Strategies
import WorkIt

import Data.Map (Map)
import Data.List (intersperse, sortBy)
import System.IO (writeFile)

import qualified Data.Map as Map

populationToTest = Population 100
thresholdTestCount = 90

main :: IO ()
main =
    let
        rates   = InfectionRate <$> [0.05, 0.1..0.30]
        pools   = PoolSize      <$> [2..20]
    in
    writeFile "stats.csv"
        $ toCsv thresholdTestCount
        $ fmap (populationStats populationToTest)
        $ assessOneLevel pools rates runTestIPChildren

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
    , sequentialApplications buckets probs
    )
    where
        buckets = population `div` degree


newtype Population = Population Int


type Row = (Population, InfectionRate, PoolSize, Map Int Probability)

toCsv :: Int -> [Row] -> String
toCsv threshold rows
  = unlines
    $ fmap (concat . intersperse ",")
    $ (:) [ "Population"
        , "InfectionRate"
        , "Group size"
        , "Probability of using less than " <> show threshold <> " tests"
        ]
    $ fmap toRow
    $ sortBy (\(_,r1,s1,_) (_,r2,s2,_) -> compare r1 r2 )
    $ sortBy (\(_,r1,s1,_) (_,r2,s2,_) -> compare s1 s2 )
    $ rows

    where
        toRow (Population population, InfectionRate i, PoolSize s, probs) =
            let Probability p = probabilityOfUsingLessThanNTests threshold probs
            in
            [show population, showAsPercentage i, show s, showAsPercentage p]


probabilityOfUsingLessThanNTests :: Int -> Map Int Probability -> Probability
probabilityOfUsingLessThanNTests n probs
    = sum
    $ map snd
    $ filter ((< n) . fst)
    $ Map.toList probs

