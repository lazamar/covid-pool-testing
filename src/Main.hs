{-# LANGUAGE RankNTypes #-}

module Main where

import Strategies
import WorkIt

main :: IO ()
main = putStrLn
    $ unlines
    [ tryIt runTestLeaves
    , tryIt runOddStrategy
    ]
    where
        tryIt :: Strategy s => (forall a. s a -> a) -> String
        tryIt run =
            let
                arities = Arity <$> [3..5]
                rates   = InfectionRate <$> [0.1]
                pools   = PoolSize <$> [7]
            in
            unlines
                [ "----------" <> run strategyName <> "---------------"
                , unlines $ showStats <$> assess arities rates pools run
                ]
