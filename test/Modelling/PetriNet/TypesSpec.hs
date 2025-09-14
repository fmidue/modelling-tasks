{-# LANGUAGE FlexibleInstances #-}
module Modelling.PetriNet.TypesSpec where

import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  Net (..),
  Node,
  PetriLike,
  SimplePetriNet,
  checkBasicConfig,
  checkChangeConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  transformNet,
  )

import qualified Data.Map                         as M (keys)

import Data.Maybe                       (isJust, fromMaybe)
import Data.Tuple.Extra                 (uncurry3)
import Test.Hspec
import Test.Hspec.QuickCheck            (prop)
import Test.QuickCheck                  (Arbitrary (..), elements, listOf)

spec :: Spec
spec = do
  describe "checkBasicConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkBasicConfig defaultBasicConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkBasicConfig defaultBasicConfig{places = 0}
          `shouldSatisfy` isJust
  describe "checkChangeConfig" $ do
    it "checks if the input for Changes is in given boundaries" $
      checkChangeConfig defaultBasicConfig defaultChangeConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $ do
      it "rejects negative tokenChangeOverall" $
        checkChangeConfig defaultBasicConfig defaultChangeConfig{tokenChangeOverall = -1}
          `shouldSatisfy` isJust
      it "rejects odd tokenChangeOverall with equal tokensOverall values" $
        checkChangeConfig defaultBasicConfig{tokensOverall = (5,5)} defaultChangeConfig{tokenChangeOverall = 3}
          `shouldSatisfy` isJust
      it "rejects odd flowChangeOverall with equal flowOverall values" $
        checkChangeConfig defaultBasicConfig{flowOverall = (8,8)} defaultChangeConfig{flowChangeOverall = 3}
          `shouldSatisfy` isJust
      it "rejects even tokenChangeOverall with consecutive tokensOverall values" $
        checkChangeConfig defaultBasicConfig{tokensOverall = (5,6)} defaultChangeConfig{tokenChangeOverall = 2}
          `shouldSatisfy` isJust
      it "rejects even flowChangeOverall with consecutive flowOverall values" $
        checkChangeConfig defaultBasicConfig{flowOverall = (7,8)} defaultChangeConfig{flowChangeOverall = 4}
          `shouldSatisfy` isJust
    context "when provided with valid configurations" $ do
      it "accepts odd tokenChangeOverall with different tokensOverall values" $
        checkChangeConfig defaultBasicConfig{tokensOverall = (3,7)} defaultChangeConfig{tokenChangeOverall = 1}
          `shouldBe` Nothing
      it "accepts odd flowChangeOverall with different flowOverall values" $
        checkChangeConfig defaultBasicConfig{flowOverall = (5,10)} defaultChangeConfig{flowChangeOverall = 3}
          `shouldBe` Nothing
      it "accepts even tokenChangeOverall with non-consecutive tokensOverall values" $
        checkChangeConfig defaultBasicConfig{tokensOverall = (3,8)} defaultChangeConfig{tokenChangeOverall = 2}
          `shouldBe` Nothing
      it "accepts even flowChangeOverall with non-consecutive flowOverall values" $
        checkChangeConfig defaultBasicConfig{flowOverall = (5,12)} defaultChangeConfig{flowChangeOverall = 4}
          `shouldBe` Nothing
  describe "a Net" $ do
    context "with and without applying fromSimpleNet" $
      netProperties fromSimpleNet
    context "with and without applying toSimpleNet" $
      netProperties toSimpleNet
  where
    fromSimpleNet :: SimplePetriNet -> PetriLike Node String
    fromSimpleNet = transformNet
    toSimpleNet :: PetriLike Node String -> SimplePetriNet
    toSimpleNet = transformNet

netProperties
  :: (Arbitrary (p1 n1 String), Eq (p2 n2 String), Net p1 n1, Net p2 n2)
  => (p1 n1 String -> p2 n2 String)
  -> SpecWith ()
netProperties transform =
  context "behaves the same for" $ do
    prop "emptyNet" $ emptyNet == transform emptyNet
    prop "nodes" $ \n -> M.keys (nodes n) == M.keys (nodes $ transform n)
    prop "flow" $ \x y n -> flow x y n == flow x y (transform n)
    prop "deleteFlow" $ \x y n -> transform (deleteFlow x y n) == deleteFlow x y (transform n)
    prop "deleteNode" $ \x n -> transform (deleteNode x n) == deleteNode x (transform n)
    prop "outFlow" $ \x n -> outFlow x n == outFlow x (transform n)
    prop "mapNet" $ \n ->
      let f = fromMaybe "" . maybeReverseNames n
      in transform (mapNet f n) == mapNet f (transform n)
    prop "traverseNet" $ \n ->
      let f = maybeReverseNames n
      in fmap transform (traverseNet f n) == traverseNet f (transform n)

maybeReverseNames :: (Net p n, Ord b) => p n b -> b -> Maybe b
maybeReverseNames n x =
  let xs = M.keys (nodes n)
  in lookup x (zip xs $ reverse xs)

instance (Arbitrary a, Net p n, Ord a) => Arbitrary (p n a) where
  arbitrary = do
    ns <- listOf arbitrary
    if null ns
      then return emptyNet
      else do
      let names = map fst ns
      flows <- listOf $ (,,)
        <$> elements names
        <*> (abs <$> arbitrary)
        <*> elements names
      return
        . flip (foldr (uncurry3 alterFlow)) flows
        . flip (foldr (uncurry alterNode)) ns
        $ emptyNet
  shrink x = map (`deleteNode` x) names
    where
      names = M.keys (nodes x)
