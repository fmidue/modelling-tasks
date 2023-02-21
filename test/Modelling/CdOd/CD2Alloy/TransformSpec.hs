module Modelling.CdOd.CD2Alloy.TransformSpec where

import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  transform,
  )
import Modelling.CdOd.Types             (ObjectConfig (..))
import Modelling.Common                 (withUnitTests)

import Test.Hspec

spec :: Spec
spec = do
  withUnitTests "transform" does dir "als" $ shouldBe . getResult . read
  where
    does = "generates expected Alloy code"
    dir = "test/unit/Modelling/CdOd/CD2Alloy/Transform/"
    getResult cd = combineParts
      $ transform cd [] objectConfig (Just True) False "1" "-"
    objectConfig = ObjectConfig {
      links          = (4, Just 10),
      linksPerObject = (0, Just 4),
      objects        = (2, 4)
      }