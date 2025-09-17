module Modelling.PetriNet.Reach.DeadlockSpec where

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  generateDeadlock,
  checkDeadlockConfig,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type    (Net (transitions))

import Data.Maybe                       (isJust)
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

import Test.Hspec
import Test.QuickCheck                  (Testable (property))

spec :: Spec
spec = do
  describe "generateDeadlock" $
    it "abides minTransitionLength" $
      property $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            ts = transitions net
        net `shouldSatisfy`
          hasMinTransitionLength (null . successors net) ts minL

  describe "checkDeadlockConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultDeadlockConfig
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects conflicting length hint configuration" $ do
      let config = defaultDeadlockConfig {
            maxTransitionLength = 8,
            rejectLongerThan = Just 8,
            showLengthHint = True
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts non-conflicting length hint configuration" $ do
      let config = defaultDeadlockConfig {
            maxTransitionLength = 8,
            minTransitionLength = 6,
            rejectLongerThan = Just 7,
            showLengthHint = True
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects negative numPlaces" $ do
      let config = defaultDeadlockConfig { numPlaces = -1 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects zero numPlaces" $ do
      let config = defaultDeadlockConfig { numPlaces = 0 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative numTransitions" $ do
      let config = defaultDeadlockConfig { numTransitions = -1 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects zero numTransitions" $ do
      let config = defaultDeadlockConfig { numTransitions = 0 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative minTransitionLength" $ do
      let config = defaultDeadlockConfig { minTransitionLength = -1 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects zero minTransitionLength" $ do
      let config = defaultDeadlockConfig { minTransitionLength = 0 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative maxTransitionLength" $ do
      let config = defaultDeadlockConfig { maxTransitionLength = -1 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects zero maxTransitionLength" $ do
      let config = defaultDeadlockConfig { maxTransitionLength = 0 }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects minTransitionLength > maxTransitionLength" $ do
      let config = defaultDeadlockConfig {
            minTransitionLength = 10,
            maxTransitionLength = 5
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative preconditionsRange lower bound" $ do
      let config = defaultDeadlockConfig { preconditionsRange = (-1, Nothing) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative postconditionsRange lower bound" $ do
      let config = defaultDeadlockConfig { postconditionsRange = (-1, Nothing) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects preconditionsRange where upper < lower" $ do
      let config = defaultDeadlockConfig { preconditionsRange = (5, Just 2) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects postconditionsRange where upper < lower" $ do
      let config = defaultDeadlockConfig { postconditionsRange = (5, Just 2) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects empty drawCommands" $ do
      let config = defaultDeadlockConfig { drawCommands = [] }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects negative rejectLongerThan" $ do
      let config = defaultDeadlockConfig {
            rejectLongerThan = Just (-1),
            showLengthHint = False
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects zero rejectLongerThan" $ do
      let config = defaultDeadlockConfig {
            rejectLongerThan = Just 0,
            showLengthHint = False
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects rejectLongerThan < minTransitionLength" $ do
      let config = defaultDeadlockConfig {
            minTransitionLength = 10,
            rejectLongerThan = Just 5,
            showLengthHint = False
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts rejectLongerThan = minTransitionLength" $ do
      let config = defaultDeadlockConfig {
            minTransitionLength = 10,
            rejectLongerThan = Just 10,
            showLengthHint = False
            }
      checkDeadlockConfig config `shouldBe` Nothing
