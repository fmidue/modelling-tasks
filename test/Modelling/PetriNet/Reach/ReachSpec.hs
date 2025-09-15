module Modelling.PetriNet.Reach.ReachSpec where

import qualified Data.Set                         as S

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Reach (
  ReachConfig (..),
  NetGoalConfig (..),
  ReachInstance (..),
  NetGoal (..),
  defaultReachConfig,
  generateReach,
  netGoalSolution,
  )
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  defaultFilterConfig,
  isTrivialSequence,
  )
import Modelling.PetriNet.Reach.Property (
  satisfiesAtAnyState,
  )
import Modelling.PetriNet.Reach.Type (
  Net (transitions),
  State,
  Transition (..),
  )

import Data.Set                         (Set)
import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec = do
  describe "generateReach" $ do
    it "abides minTransitionLength" $
      property $ \seed -> do
        let config = defaultReachConfig {
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxTransitionLength = 6,
                minTransitionLength = 6
                }
              }
            minL = minTransitionLength (netGoalConfig config)
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            s = goal (netGoal inst)
            ts = transitions net
        net `shouldSatisfy` hasMinTransitionLength (s ==) ts minL

    it "generates non-trivial solutions when filtering is enabled" $
      property $ \seed -> do
        let config = defaultReachConfig {
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxTransitionLength = 8,
                minTransitionLength = 8
                },
              filterConfig = defaultFilterConfig
              }
        inst <- generateReach config seed
        let solution = netGoalSolution (netGoal inst)
        solution `shouldSatisfy` (not . isTrivialSequence (filterConfig config))

    it "can generate solutions when filtering is disabled" $
      property $ \seed -> do
        let config = defaultReachConfig {
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxTransitionLength = 8,
                minTransitionLength = 8
                },
              filterConfig = FilterConfig False False False 3 4  -- All filtering disabled
              }
        inst <- generateReach config seed
        let solution = netGoalSolution (netGoal inst)
        length solution `shouldBe` 8

hasMinTransitionLength
  :: (Ord s, Show s)
  => (State s -> Bool)
  -> Set Transition
  -> Int
  -> Net s Transition
  -> Bool
hasMinTransitionLength p ts minL n =
  not (any (satisfiesAtAnyState p n) variants)
  where
    variants = transitionVariants $ minL - 1
    transitionVariants x
      | x < 1     = [[]]
      | otherwise = [ a : as |
          a <- S.toList ts,
          as <- transitionVariants (x-1)
          ]
