-- | Common validation logic for Petri Net configurations (Deadlock and Reach)
module Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkRange,
  checkPetriNetSizes,
  checkTransitionLengths,
  checkRejectLongerThanConsistency
) where

import Control.Applicative (Alternative ((<|>)))
import Data.GraphViz.Commands (GraphvizCommand)

-- | Check that a range (low, high) is valid
checkRange
  :: (Num n, Ord n, Show b, Show n)
  => (b -> Maybe n)  -- ^ Function to extract upper bound
  -> String          -- ^ Description of what is being checked
  -> (n, b)          -- ^ (lower bound, upper bound)
  -> Maybe String
checkRange g what (low, h) = do
  high <- g h
  assert high
  where
    assert high
      | low < 0 = Just $ "The lower limit for " ++ what ++ " has to be at least 0!"
      | high < low = Just $ 
        "The upper limit (currently " ++ show h ++ "; second value) for " ++ what ++
        " has to be at least as high as its lower limit (currently " ++ show low ++ "; first value)!"
      | otherwise = Nothing

-- | Check basic Petri net size constraints
checkPetriNetSizes :: Int -> Int -> Maybe String
checkPetriNetSizes numPlaces numTransitions
  | numPlaces <= 0 = Just "numPlaces must be positive"
  | numTransitions <= 0 = Just "numTransitions must be positive"
  | otherwise = Nothing

-- | Check transition length constraints
checkTransitionLengths :: Int -> Int -> Maybe String
checkTransitionLengths minTransitionLength maxTransitionLength
  | minTransitionLength <= 0 = Just "minTransitionLength must be positive"
  | maxTransitionLength <= 0 = Just "maxTransitionLength must be positive"
  | minTransitionLength > maxTransitionLength = Just $
    "minTransitionLength (" ++ show minTransitionLength ++ ") cannot be greater than maxTransitionLength (" ++ show maxTransitionLength ++ ")"
  | otherwise = Nothing

-- | Check consistency between rejectLongerThan and other length parameters
checkRejectLongerThanConsistency :: Maybe Int -> Int -> Int -> Maybe String
checkRejectLongerThanConsistency rejectLongerThan minTransitionLength _maxTransitionLength =
  case rejectLongerThan of
    Just rejectLength
      | rejectLength <= 0 -> Just "rejectLongerThan must be positive when specified"
      | rejectLength < minTransitionLength -> Just $
        "rejectLongerThan (" ++ show rejectLength ++ ") cannot be less than minTransitionLength (" ++ show minTransitionLength ++ ")"
      | otherwise -> Nothing
    Nothing -> Nothing

-- | Check basic Petri net configuration including sizes, lengths, ranges and draw commands
checkBasicPetriConfig 
  :: Int                      -- ^ numPlaces
  -> Int                      -- ^ numTransitions  
  -> Int                      -- ^ minTransitionLength
  -> Int                      -- ^ maxTransitionLength
  -> (Int, Maybe Int)         -- ^ preconditionsRange
  -> (Int, Maybe Int)         -- ^ postconditionsRange
  -> [GraphvizCommand]        -- ^ drawCommands
  -> Maybe Int                -- ^ rejectLongerThan
  -> Maybe String
checkBasicPetriConfig 
  numPlaces 
  numTransitions 
  minTransitionLength 
  maxTransitionLength 
  preconditionsRange 
  postconditionsRange 
  drawCommands 
  rejectLongerThan =
    checkPetriNetSizes numPlaces numTransitions
    <|> checkTransitionLengths minTransitionLength maxTransitionLength
    <|> checkRange id "preconditionsRange" preconditionsRange
    <|> checkRange id "postconditionsRange" postconditionsRange
    <|> checkRejectLongerThanConsistency rejectLongerThan minTransitionLength maxTransitionLength
    <|> checkDrawCommands drawCommands
  where
    checkDrawCommands [] = Just "drawCommands cannot be empty"
    checkDrawCommands _  = Nothing
