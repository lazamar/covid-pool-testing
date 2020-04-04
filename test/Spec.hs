import Test.Hspec (Spec, describe, it, shouldBe, hspec, xdescribe, xit)
import Test.QuickCheck (suchThat, property, vectorOf, Arbitrary, arbitrary, arbitraryBoundedEnum, NonEmptyList(..))
import Strategies
import Data.Tree (Tree(..))

maxSampleSize = 10

instance Arbitrary Arity where
    arbitrary = Arity <$> suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)

instance Arbitrary Condition where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InfectionRate where
    arbitrary = InfectionRate <$> suchThat arbitrary (\n -> n > 0 && n < 1)

newtype SampleSize = SampleSize Int
    deriving (Show, Eq)

instance Arbitrary SampleSize where
    arbitrary = SampleSize <$> suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)

fromLikelihood :: Likelihood -> Double
fromLikelihood (Likelihood v) = v

isEqualWithTolerance :: Double -> Double -> Double -> Bool
isEqualWithTolerance tolerance reference result =
    reference - tolerance < result && result < reference + tolerance


main :: IO ()
main = hspec $ do
    describe "Tree generation" $ do
         it "likelihoods add up to 100%" $ do
             property $ \rate (SampleSize sampleSize) arity ->
                 -- We have some tolerance to account for
                 -- the innaccuracy of floating number arithmetic.
                 isEqualWithTolerance 0.01 1
                     $ sum
                     $ fmap (fromLikelihood . fst)
                     $ generateTrees rate sampleSize arity

         it "doesn't miss any scenario" $ do
            property $ do
                n <- suchThat arbitrary (\n -> n > 0 && n < maxSampleSize)
                -- create a random scenario
                scenario <- vectorOf n arbitrary
                -- Make sure it appears in generateScenarios
                return $ scenario `elem` generateScenarios (length scenario)
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
