module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (
  SelectASConfig(..),
  SelectASInstance(..),
  checkSelectASConfig,
  checkSelectASInstanceForConfig,
  defaultSelectASConfig,
  defaultSelectASInstance
  )

import qualified Data.Map as M (fromList)

import Modelling.ActivityDiagram.Config (
  AdConfig (objectNodeLimits),
  defaultAdConfig,
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust, isNothing)

spec :: Spec
spec = do
  describe "checkSelectASConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectASConfig defaultSelectASConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkSelectASConfig defaultSelectASConfig {
          adConfig = defaultAdConfig {objectNodeLimits = (0, 1)},
          objectNodeOnEveryPath = Just True
        } `shouldSatisfy` isJust

  describe "checkSelectASInstanceForConfig" $ do
    it "accepts instance when all sequences are within answerLength bounds" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance
        defaultSelectASConfig
      `shouldSatisfy` isNothing

    it "rejects instance when the correct sequence is too short" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance {
          actionSequences = M.fromList [
            (1, (True, ["A", "B"])),
            (2, (False, ["A", "B", "C", "D", "E"]))
          ]
        }
        defaultSelectASConfig { answerLength = (4, 8) }
      `shouldSatisfy` isJust

    it "rejects instance when a wrong sequence is too short" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance {
          actionSequences = M.fromList [
            (1, (True, ["A", "B", "C", "D", "E"])),
            (2, (False, ["A", "B"]))
          ]
        }
        defaultSelectASConfig { answerLength = (4, 8) }
      `shouldSatisfy` isJust

    it "rejects instance when the correct sequence is too long" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance {
          actionSequences = M.fromList [
            (1, (True, ["A", "B", "C", "D", "E", "F", "G", "H", "I"])),
            (2, (False, ["A", "B", "C", "D", "E"]))
          ]
        }
        defaultSelectASConfig { answerLength = (4, 8) }
      `shouldSatisfy` isJust

    it "rejects instance when a wrong sequence is too long" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance {
          actionSequences = M.fromList [
            (1, (True, ["A", "B", "C", "D", "E"])),
            (2, (False, ["A", "B", "C", "D", "E", "F", "G", "H", "I"]))
          ]
        }
        defaultSelectASConfig { answerLength = (4, 8) }
      `shouldSatisfy` isJust

    it "accepts instance when all sequences (both correct and wrong) are within bounds" $
      checkSelectASInstanceForConfig
        defaultSelectASInstance {
          actionSequences = M.fromList [
            (1, (True, ["A", "B", "C", "D", "E"])),
            (2, (False, ["A", "B", "C", "D", "E", "F"])),
            (3, (False, ["A", "B", "C", "D"]))
          ]
        }
        defaultSelectASConfig { answerLength = (4, 8) }
      `shouldSatisfy` isNothing
