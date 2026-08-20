module Modelling.ActivityDiagram.SelectPetriSpec where

import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..), checkSelectPetriConfig, defaultSelectPetriConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, objectNodeLimits, forkJoinPairs, activityFinalNodes, flowFinalNodes),
  defaultAdConfig,
  )


spec :: Spec
spec =
  describe "checkSelectPetriConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectPetriConfig defaultSelectPetriConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkSelectPetriConfig defaultSelectPetriConfig {
          adConfig = defaultAdConfig {
            actionLimits = (0, 4),
            forkJoinPairs = 0
            },
          presenceOfSinkTransitionsForFinals = Just False
          }
            `shouldSatisfy` isJust
    context "when countOfPetriNodesBounds minimum is too small for AdConfig" $
      it "returns validation error about minimum Petri net nodes" $
        checkSelectPetriConfig defaultSelectPetriConfig {
          adConfig = defaultAdConfig {
            actionLimits = (3, 5),
            activityFinalNodes = 1,
            flowFinalNodes = 0
            },
          countOfPetriNodesBounds = (1, Nothing)  -- Too small for minimum calculation
          }
            `shouldSatisfy` isJust
    context "when countOfPetriNodesBounds maximum is too small for AdConfig" $
      it "returns validation error about maximum Petri net nodes being too small" $
        checkSelectPetriConfig defaultSelectPetriConfig {
          adConfig = defaultAdConfig {
            actionLimits = (1, 5),  -- Can achieve up to 5 actions
            objectNodeLimits = (0, 3),  -- Can achieve up to 3 objects
            activityFinalNodes = 1,
            flowFinalNodes = 0
            },
          countOfPetriNodesBounds = (1, Just 8)  -- Too small - achievable is 1+5+3+1 = 10 standard + ~10 auxiliary = ~20
          }
            `shouldSatisfy` isJust
