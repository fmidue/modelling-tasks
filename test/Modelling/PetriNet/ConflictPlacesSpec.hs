module Modelling.PetriNet.ConflictPlacesSpec where

import Modelling.Common (runWithoutOutput)
import Modelling.PetriNet.ConflictPlaces (
  checkFindConflictPlacesConfig,
  defaultFindConflictPlacesConfig,
  defaultFindConflictPlacesInstance,
  checkFindConflictInstance,
  )

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
  describe "checkFindConflictPlacesConfig" $
    it "accepts the default config" $
      checkFindConflictPlacesConfig defaultFindConflictPlacesConfig
      `shouldBe` Nothing
  describe "defaultFindConflictPlacesInstance" $
    it "passes checkFindConflictInstance" $ do
      runWithoutOutput (checkFindConflictInstance defaultFindConflictPlacesInstance)
      `shouldReturn` Just ()
