module Strategies where

import Data.Bifunctor (first, second)
import Data.List (mapAccumL)

main =
    putStrLn "Hello"

data Condition
    = Infected
    | Healthy

data Tree a
    = Node [Tree a]
    | Leaf a

data ResultTree
    -- The (Maybe Condition) will be Nothing if the node or leaf was not tested
    = RNode (Maybe Condition) [ResultTree]
    | RLeaf (Maybe Condition)


data EvaluationMode
    = Sequential
    | Parallel

data Info = Info
    { arity :: Int
    , poolSize :: Int
    , evaluationMode :: EvaluationMode
    }

data Strategy state = Strategy

newtype Arity = Arity Int

-- | Evaluate a tree according to a strategy
evaluateTree :: Strategy state -> Info -> state -> Tree Condition -> (state, ResultTree)
evaluateTree strategy info state tree =
    case tree of
        Leaf condition ->
            second RLeaf $ evaluateNode strategy info state condition

        Node subtrees ->
            let (nodeState, nodeResult) = evaluateNode strategy info state (test tree)

                (childrenState, childrenNodes) =
                    evaluateLevel
                        (evaluationMode info)
                        (evaluateTree strategy info)
                        nodeState
                        subtrees
            in
            (childrenState, RNode nodeResult childrenNodes)

evaluateLevel :: EvaluationMode -> (state -> input -> (state, output)) -> state -> [input] -> (state, [output])
evaluateLevel mode f state list =
    case mode of
      Sequential -> mapAccumL f state list
      Parallel   -> (state, fmap (snd . f state) list)



-- | Mix the samples of everyone in this tree
-- and return a result
test :: Tree Condition -> Condition
test = undefined

isValid :: Tree Condition -> ResultTree -> Bool
isValid = undefined

-- | By being polymorphic on the response condition, we prevent
-- this function from accidentally misdiagnosing the input.
evaluateNode
    :: Strategy state
    -> Info
    -> state
    -> condition
    -> (state, Maybe condition)
evaluateNode strategy parents siblings = undefined


