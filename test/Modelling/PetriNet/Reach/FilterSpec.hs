module Modelling.PetriNet.Reach.FilterSpec where

import Modelling.PetriNet.Reach.Filter
import Modelling.PetriNet.Reach.Type (Transition(..))

import Test.Hspec

spec :: Spec
spec = do
  describe "isCyclicPattern" $ do
    it "detects simple cyclic patterns" $ do
      let pattern = [Transition 1, Transition 2, Transition 3, Transition 4]
      isCyclicPattern 4 (pattern ++ pattern) `shouldBe` True
      
    it "detects cyclic patterns with multiple repetitions" $ do
      let pattern = [Transition 1, Transition 2]
      isCyclicPattern 4 (pattern ++ pattern ++ pattern ++ pattern) `shouldBe` True
      
    it "does not detect partial cycles" $ do
      let pattern = [Transition 1, Transition 2, Transition 3, Transition 4]
      isCyclicPattern 4 (pattern ++ [Transition 1, Transition 2]) `shouldBe` False
      
    it "does not detect single cycles" $ do
      let pattern = [Transition 1, Transition 2, Transition 3, Transition 4]
      isCyclicPattern 4 pattern `shouldBe` False
      
    it "handles empty lists" $ do
      isCyclicPattern 4 ([] :: [Transition]) `shouldBe` False
      
    it "handles short lists" $ do
      isCyclicPattern 4 [Transition 1, Transition 2] `shouldBe` False

  describe "hasRepetitiveSubsequence" $ do
    it "detects repetitive prefixes" $ do
      hasRepetitiveSubsequence 3 [Transition 1, Transition 1, Transition 1, Transition 2] `shouldBe` True
      hasRepetitiveSubsequence 4 [Transition 4, Transition 4, Transition 4, Transition 4] `shouldBe` True
      
    it "detects repetitive suffixes" $ do
      hasRepetitiveSubsequence 3 [Transition 1, Transition 2, Transition 3, Transition 3, Transition 3] `shouldBe` True
      
    it "does not detect short repetitions" $ do
      hasRepetitiveSubsequence 3 [Transition 1, Transition 1, Transition 2] `shouldBe` False
      
    it "handles minimum length requirement" $ do
      hasRepetitiveSubsequence 5 [Transition 1, Transition 1, Transition 1] `shouldBe` False
      hasRepetitiveSubsequence 2 [Transition 1, Transition 1] `shouldBe` True

  describe "hasGroupedRepeats" $ do
    it "detects grouped repeats pattern" $ do
      hasGroupedRepeats [Transition 1, Transition 1, Transition 2, Transition 2, 
                         Transition 3, Transition 3, Transition 4, Transition 4] `shouldBe` True
      
    it "detects grouped repeats with different group sizes" $ do
      hasGroupedRepeats [Transition 1, Transition 1, Transition 1, 
                         Transition 2, Transition 2, Transition 2] `shouldBe` True
      
    it "does not detect single elements" $ do
      hasGroupedRepeats [Transition 1, Transition 2, Transition 3, Transition 4] `shouldBe` False
      
    it "does not detect mixed patterns" $ do
      hasGroupedRepeats [Transition 1, Transition 1, Transition 2, 
                         Transition 3, Transition 3, Transition 3] `shouldBe` False
      
    it "handles empty and short lists" $ do
      hasGroupedRepeats ([] :: [Transition]) `shouldBe` False
      hasGroupedRepeats [Transition 1, Transition 2] `shouldBe` False

  describe "isTrivialSequence" $ do
    it "detects the Spaceballs PIN pattern" $ do
      let spaceballsPattern = [Transition 1, Transition 2, Transition 3, Transition 4,
                               Transition 1, Transition 2, Transition 3, Transition 4]
      isTrivialSequence defaultFilterConfig spaceballsPattern `shouldBe` True
      
    it "detects repetitive patterns" $ do
      let repetitivePattern = [Transition 4, Transition 4, Transition 4, Transition 4]
      isTrivialSequence defaultFilterConfig repetitivePattern `shouldBe` True
      
    it "detects grouped repeats" $ do
      let groupedPattern = [Transition 1, Transition 1, Transition 2, Transition 2,
                            Transition 3, Transition 3, Transition 4, Transition 4]
      isTrivialSequence defaultFilterConfig groupedPattern `shouldBe` True
      
    it "does not flag legitimate sequences" $ do
      let legitimatePattern = [Transition 1, Transition 3, Transition 2, Transition 2,
                               Transition 4, Transition 3, Transition 1, Transition 1]
      isTrivialSequence defaultFilterConfig legitimatePattern `shouldBe` False

  describe "filterTrivialSolutions" $ do
    it "filters out trivial solutions while keeping legitimate ones" $ do
      let trivial1 = [Transition 1, Transition 2, Transition 3, Transition 4,
                      Transition 1, Transition 2, Transition 3, Transition 4]
      let trivial2 = [Transition 4, Transition 4, Transition 4, Transition 4]
      let legitimate = [Transition 1, Transition 3, Transition 2, Transition 4,
                        Transition 1, Transition 3, Transition 2, Transition 1]
      let solutions = [trivial1, legitimate, trivial2]
      filterTrivialSolutions defaultFilterConfig solutions `shouldBe` [legitimate]

  describe "configuration" $ do
    it "respects filter configuration settings" $ do
      let cyclicPattern = [Transition 1, Transition 2, Transition 1, Transition 2]
      let configNoCyclic = defaultFilterConfig { filterCyclicPatterns = False }
      isTrivialSequence configNoCyclic cyclicPattern `shouldBe` False
      isTrivialSequence defaultFilterConfig cyclicPattern `shouldBe` True