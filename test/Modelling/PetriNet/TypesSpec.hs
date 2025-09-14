{-# LANGUAGE FlexibleInstances #-}
module Modelling.PetriNet.TypesSpec where

import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  Net (..),
  Node,
  PetriLike,
  SimplePetriNet,
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  defaultBasicConfig,
  defaultChangeConfig,
  defaultGraphConfig,
  transformNet,
  )
import Data.GraphViz.Attributes.Complete (GraphvizCommand (..))

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
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkChangeConfig defaultBasicConfig defaultChangeConfig{tokenChangeOverall = -1}
          `shouldSatisfy` isJust
  describe "checkGraphLayouts" $ do
    let testConfig n = defaultGraphConfig { graphLayouts = take n [Dot, Neato, TwoPi, Circo] }
    context "when useDifferentGraphLayouts is False" $ do
      it "accepts any valid configuration" $ do
        checkGraphLayouts False 4 (testConfig 3) `shouldBe` Nothing
        checkGraphLayouts False 10 (testConfig 2) `shouldBe` Nothing
    context "when useDifferentGraphLayouts is True (new behavior)" $ do
      it "accepts configuration when numberOfGraphs mod n == 0 for some n <= numberOfLayouts" $ do
        -- 4 graphs, 3 layouts: 4 mod 2 == 0, and 2 <= 3, so should be valid
        checkGraphLayouts True 4 (testConfig 3) `shouldBe` Nothing
        -- 6 graphs, 3 layouts: 6 mod 3 == 0, and 3 <= 3, so should be valid  
        checkGraphLayouts True 6 (testConfig 3) `shouldBe` Nothing
        -- 8 graphs, 4 layouts: 8 mod 4 == 0, and 4 <= 4, so should be valid
        checkGraphLayouts True 8 (testConfig 4) `shouldBe` Nothing
        -- 9 graphs, 3 layouts: 9 mod 3 == 0, and 3 <= 3, so should be valid
        checkGraphLayouts True 9 (testConfig 3) `shouldBe` Nothing
      it "rejects configuration when numberOfGraphs is not divisible by any valid n" $ do
        -- 5 graphs, 3 layouts: 5 mod 2 == 1, 5 mod 3 == 2, so no valid n
        checkGraphLayouts True 5 (testConfig 3) `shouldSatisfy` isJust
        -- 7 graphs, 3 layouts: 7 mod 2 == 1, 7 mod 3 == 1, so no valid n
        checkGraphLayouts True 7 (testConfig 3) `shouldSatisfy` isJust
      it "rejects configuration with empty graphLayouts" $
        checkGraphLayouts True 4 defaultGraphConfig{graphLayouts = []} `shouldSatisfy` isJust
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
