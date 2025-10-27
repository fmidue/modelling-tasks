{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.Reach.FilterSpec where

import Modelling.PetriNet.Reach.Filter
import Modelling.PetriNet.Reach.Type (Transition(..))

import Data.List                        (zipWith4, zipWith5, zipWith6, zipWith7)
import Data.List.Extra                  (nubOrd)
import Test.Hspec
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Testable (property),
  (==>),
  chooseInt,
  forAll,
  suchThat,
  vectorOf,
  )

genNub :: (Arbitrary a, Ord a) => Gen [a]
genNub = nubOrd <$> arbitrary

genNubSized :: (Arbitrary a, Ord a) => Int -> Gen [a]
genNubSized n = vectorOf n arbitrary `suchThat` noDuplicates
  where
    noDuplicates xs = length xs == length (nubOrd xs)

zipN :: Int -> [a] -> [a]
zipN n xs = concat $ case n of
  2 -> zipWith (\as bs -> [as, bs]) xs xs
  3 -> zipWith3 (\as bs cs -> [as, bs, cs]) xs xs xs
  4 -> zipWith4 (\as bs cs ds -> [as, bs, cs, ds]) xs xs xs xs
  5 -> zipWith5 (\as bs cs ds es -> [as, bs, cs, ds, es]) xs xs xs xs xs
  6 -> zipWith6
    (\as bs cs ds es fs -> [as, bs, cs, ds, es, fs])
    xs xs xs xs xs xs
  7 -> zipWith7
    (\as bs cs ds es fs gs -> [as, bs, cs, ds, es, fs, gs])
    xs xs xs xs xs xs xs
  _ -> error $ "zipN is for parameter " ++ show n ++ " not defined"

spec :: Spec
spec = do
  describe "isCyclicPattern" $ do
    it "detects cyclic patterns" $
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (m, 9)) $ \n ->
          forAll (genNubSized m) $
            isCyclicPattern @Int n . concat . replicate 2

    it "does not detect too large cyclic patterns" $
      forAll (chooseInt (3, 9)) $ \m ->
        forAll (chooseInt (2, m - 1)) $ \n ->
          forAll (genNubSized m) $
            not . isCyclicPattern @Int n . concat . replicate 2

    it "does only detect complete cyclic patterns" $
      forAll (chooseInt (0, 9)) $ \m ->
        forAll (chooseInt (0, 9)) $ \n ->
          forAll (genNubSized m) $ \xs ->
            forAll (genNubSized n) $ \ys ->
              xs /= ys ==> not $ isCyclicPattern @Int (m + n) $ xs ++ ys

  describe "hasRepetitiveSubsequence" $ do
    it "detects repetitive prefixes" $
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (m, 9)) $ \n ->
          forAll (genNubSized m) $ \xs ->
            hasRepetitiveSubsequence @Int m $ replicate (n - 1) (head xs) ++ xs

    it "detects repetitive suffixes" $
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (m, 9)) $ \n ->
          forAll (genNubSized m) $ \xs ->
            hasRepetitiveSubsequence @Int m $ xs ++ replicate (n - 1) (last xs)

    it "does not detect too short repetitions" $ do
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (0, m - 1)) $ \n ->
          forAll (genNubSized m) $ \xs ->
            not $ hasRepetitiveSubsequence @Int m $
              replicate (n - 1) (head xs) ++ xs ++ replicate (n - 1) (last xs)

  describe "hasGroupedRepeats" $ do
    it "detects grouped repeats pattern" $ do
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (2, 7)) $ \n ->
          forAll (genNubSized m) $ \xs ->
            length xs >= 2 ==> hasGroupedRepeats @Int (zipN n xs)

    it "does not detect intersepted grouped repeats" $ do
      forAll (chooseInt (2, 9)) $ \m ->
        forAll (chooseInt (2, 7)) $ \n ->
          forAll (chooseInt (0, m * n)) $ \i ->
            forAll (genNubSized m) $ \xs ->
              property $ \x -> not $ hasGroupedRepeats @Int
                (let (front, end) = splitAt i (zipN n xs) in front ++ x : end)

  describe "configuration" $ do
    it "respects filter configuration settings" $ do
      let cyclicPattern = [Transition 1, Transition 2, Transition 1, Transition 2]
      let configNoCyclic = defaultFilterConfig { filterCyclicPatterns = False }
      isTrivialSequence configNoCyclic cyclicPattern `shouldBe` False
      isTrivialSequence defaultFilterConfig cyclicPattern `shouldBe` True
