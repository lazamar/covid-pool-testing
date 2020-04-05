{-# LANGUAGE RankNTypes #-}

module Main where

import Strategies
import WorkIt

main :: IO ()
main = putStrLn
    $ unlines
    [ tryIt runTestIPChildren ]
    where
        tryIt :: Strategy s => (forall a. s a -> a) -> String
        tryIt run =
            let
                rates   = InfectionRate <$> [0.2]
                pools   = PoolSize      <$> [2..20]
            in
            unlines
                [ "----------" <> run strategyName <> "---------------"
                , unlines $ showStats <$> assessOneLevel pools rates run
                ]
