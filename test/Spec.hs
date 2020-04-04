import Test.Hspec (Spec, describe, it, shouldBe, hspec)
import Test.QuickCheck (suchThat, property, vectorOf, Arbitrary, arbitrary, arbitraryBoundedEnum, NonEmptyList(..))
import Strategies

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
