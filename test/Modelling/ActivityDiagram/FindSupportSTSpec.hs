module Modelling.ActivityDiagram.FindSupportSTSpec where

import Modelling.ActivityDiagram.FindSupportST (FindSupportSTConfig(..), checkFindSupportSTConfig, defaultFindSupportSTConfig)

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Modelling.ActivityDiagram.Config (
  AdConfig (actionLimits, forkJoinPairs),
  defaultAdConfig,
  )


spec :: Spec
spec =
  describe "checkFindSupportSTConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkFindSupportSTConfig defaultFindSupportSTConfig  `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkFindSupportSTConfig defaultFindSupportSTConfig {
          adConfig = defaultAdConfig {actionLimits = (0, 4), forkJoinPairs = 0},
          avoidAddingSinksForFinals = Just True
          }
            `shouldSatisfy` isJust
