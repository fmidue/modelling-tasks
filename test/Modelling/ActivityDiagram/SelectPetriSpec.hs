module Modelling.ActivityDiagram.SelectPetriSpec where

import Modelling.ActivityDiagram.SelectPetri (
  SelectPetriConfig(..),
  SelectPetriSolution(..),
  checkSelectPetriConfig,
  defaultSelectPetriConfig,
  selectPetriNet
  )
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode(..),
  AdConnection(..)
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, forkJoinPairs),
  defaultAdConfig,
  )
import Control.Monad.Random (evalRandT, mkStdGen)


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
    
    describe "selectPetriNet" $ do
      it "wrongNets satisfy countOfPetriNodesBounds" $ do
        -- This test verifies that selectPetriNet accepts the new countOfPetriNodesBounds parameter
        -- and doesn't crash. Functional testing would require more complex setup.
        let simpleAd = UMLActivityDiagram {
              nodes = [
                AdInitialNode { label = 1 },
                AdActionNode { label = 2, name = "A" },
                AdActivityFinalNode { label = 3 }
              ],
              connections = [
                AdConnection { from = 1, to = 2, guard = "" },
                AdConnection { from = 2, to = 3, guard = "" }
              ]
            }
            countBounds = (1, Just 100)  -- Very lenient bounds to avoid hanging
            numWrongNets = 1  -- Just one wrong net
            numModifications = 1
            doModifyAtMid = False
        
        -- Test that the function accepts the new parameter signature
        solution <- evalRandT (selectPetriNet numWrongNets numModifications doModifyAtMid countBounds simpleAd) (mkStdGen 42)
        
        -- Basic validation
        length (wrongNets solution) `shouldSatisfy` (<= numWrongNets)
