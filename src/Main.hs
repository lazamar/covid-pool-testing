{-# LANGUAGE RankNTypes #-}

module Main where

import Strategies
import WorkIt

import qualified Data.Map as Map

main :: IO ()
main = putStrLn
    $ unlines
    [ tryIt runTestIPChildren ]
    where
        tryIt :: Strategy s => (forall a. s a -> a) -> String
        tryIt run =
            let
                rates   = InfectionRate <$> [0.2]
                pools   = PoolSize      <$> [2..10]
            in
            unlines
                [ "----------" <> run strategyName <> "---------------"
                , unlines $ showStats' <$> assessOneLevel pools rates run
                ]

showStats' res@(Degree degree,_,_,prob) =
    unlines
        [ showStats res
        , unlines $ show <$> ap
        , "Total :" <> show (sum (fmap snd ap))
        ]

    where
        peopleToTest = 100
        testBuckets  = peopleToTest `div` degree
        ap = Map.toList (sequentialApplication testBuckets prob)

