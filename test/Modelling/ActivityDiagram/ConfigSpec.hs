module Modelling.ActivityDiagram.ConfigSpec where

import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)


spec :: Spec
spec = do
  describe "checkAdConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkAdConfig defaultAdConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (0, 4),
          objectNodeLimits = (0, 1)
          }
          `shouldSatisfy` isJust

    context "when maximal action and object node limits exceed maxNamedNodes" $ do
      it "rejects when max actions + min object nodes > maxNamedNodes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 3),
          objectNodeLimits = (3, 4),
          maxNamedNodes = 5
          }
          `shouldSatisfy` isJust

      it "rejects when min actions + max object nodes > maxNamedNodes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (2, 4),
          objectNodeLimits = (1, 2),
          maxNamedNodes = 4
          }
          `shouldSatisfy` isJust

      it "rejects the first problematic case from issue (maxNamedNodes too large)" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 3),
          objectNodeLimits = (3, 4),
          maxNamedNodes = 10
          }
          `shouldSatisfy` isJust

      it "rejects the second problematic case from issue" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 2),
          objectNodeLimits = (2, 4),
          maxNamedNodes = 4
          }
          `shouldSatisfy` isJust

      it "rejects the third problematic case from issue" $
        checkAdConfig defaultAdConfig {
          actionLimits = (2, 4),
          objectNodeLimits = (1, 2),
          maxNamedNodes = 4
          }
          `shouldSatisfy` isJust

    context "when action and object node limits are properly bounded" $ do
      it "accepts when limits are tight but valid" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 10),
          objectNodeLimits = (1, 10),
          maxNamedNodes = 15
          }
          `shouldBe` Nothing

      it "accepts when max actions + min object nodes = maxNamedNodes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 3),
          objectNodeLimits = (2, 4),
          maxNamedNodes = 5
          }
          `shouldBe` Nothing

      it "accepts when min actions + max object nodes = maxNamedNodes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 2),
          objectNodeLimits = (1, 2),
          maxNamedNodes = 3
          }
          `shouldBe` Nothing

      it "accepts when maxNamedNodes equals sum of max actions and max object nodes" $
        checkAdConfig defaultAdConfig {
          actionLimits = (1, 3),
          objectNodeLimits = (2, 4),
          maxNamedNodes = 7
          }
          `shouldBe` Nothing
