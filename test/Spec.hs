import Test.Hspec (Spec, describe, it, shouldBe, hspec, xdescribe, xit)
import Test.QuickCheck (suchThat, property, vectorOf, Arbitrary, arbitrary, arbitraryBoundedEnum, NonEmptyList(..))
import Strategies
import Combinatorics
import Data.Tree (Tree(..))

maxSampleSize = 10

instance Arbitrary Degree where
    arbitrary = Degree <$> suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)

instance Arbitrary Condition where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InfectionRate where
    arbitrary = InfectionRate <$> suchThat arbitrary (\n -> n > 0 && n < 1)

instance Arbitrary PoolSize where
    arbitrary = PoolSize <$> suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)

isEqualWithTolerance :: Double -> Double -> Probability -> Bool
isEqualWithTolerance tolerance reference result =
    result > Probability (reference - tolerance)
    &&
    result < Probability (reference + tolerance)

conditions :: [Condition]
conditions = [minBound..]

main :: IO ()
main = hspec $ do
    describe "Tree generation" $ do
         xit "likelihoods add up to 100%" $ do
             property $ \rate size ->
                 -- We have some tolerance to account for
                 -- the innaccuracy of floating number arithmetic.
                 isEqualWithTolerance 0.01 1
                     $ sum
                     $ fmap (pProbability (toProbabilityMap rate))
                     $ allPossibleScenarios size

         it "combinatoric likelihoods add up to 100%" $ do
             property $ \rate (PoolSize size) ->
                 -- We have some tolerance to account for
                 -- the innaccuracy of floating number arithmetic.
                 isEqualWithTolerance 0.01 1
                     $ sum
                     $ fmap (cProbability (toProbabilityMap rate))
                     $ allCombinations conditions size

         xit "doesn't miss any scenario" $ do
            property $ do
                n <- suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)
                -- create a random scenario
                scenario <- vectorOf n arbitrary
                -- Make sure it appears in generateScenarios
                return $ Permutation scenario `elem` allPossibleScenarios (PoolSize n)
    describe "Result Tree" $ do
        it "is valid when everyone is tested" $
            let
                tree = Node (Just Infected)
                    [ Node (Just Healthy) []
                    , Node (Just Infected) []
                    ]
            in
            isValid tree `shouldBe` True

        it "is valid when children of a healthy parent are not tested" $
            let
                tree = Node (Just Healthy)
                    [ Node Nothing []
                    , Node Nothing []
                    ]
            in
            isValid tree `shouldBe` True

        it "is valid when only leaves are tested" $
            let
                tree = Node Nothing
                    [ Node (Just Healthy) []
                    , Node (Just Infected) []
                    ]
            in
            isValid tree `shouldBe` True

        it "is invalid when children of infected nodes are not tested" $
            let
                tree = Node (Just Infected)
                    [ Node Nothing []
                    , Node (Just Infected) []
                    ]
            in
            isValid tree `shouldBe` False
