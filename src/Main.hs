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
    writeFile "stats.csv"
        $ toCsv thresholdTestCount
        $ fmap (populationStats populationToTest)
        $ complex
    where
        rates  = InfectionRate <$> [0.05, 0.1..0.30]

        -- simple =
        --     let pools   = PoolSize      <$> [2..20]
        --     in
        --     assessOneLevel pools rates runTestIPChildren

        complex =
            let pools = PoolSize <$> [2..7]
                degrees = Degree <$> [2..7]
            in
            assess degrees rates pools runTestIPChildren


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

toCsv :: Int -> [Row] -> String
toCsv threshold rows
  = unlines
    $ fmap (concat . intersperse ",")
    $ (:) [ "Population"
        , "InfectionRate"
        , "Group size"
        , "Tree degree"
        , "Probability of using less than " <> show threshold <> " tests"
        ]
    $ fmap toRow
    $ sortBy (\(_,r1,_,_,_) (_,r2,_,_,_) -> compare r1 r2 )
    $ sortBy (\(_,_,s1,_,_) (_,_,s2,_,_) -> compare s1 s2 )
    $ sortBy (\(_,_,_,d1,_) (_,_,_,d2,_) -> compare d1 d2 )
    $ rows

    where
        toRow (Population population, InfectionRate i, PoolSize s, Degree d, probs) =
            let Probability p = probabilityOfUsingLessThanNTests threshold probs
            in
            [show population, showAsPercentage i, show s, show d, showAsPercentage p]


probabilityOfUsingLessThanNTests :: Int -> Map Int Probability -> Probability
probabilityOfUsingLessThanNTests n probs
    = sum
    $ map snd
    $ filter ((< n) . fst)
    $ Map.toList probs

