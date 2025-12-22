{-# LANGUAGE RecordWildCards #-}

-- | Common validation logic for Petri Net configurations (Deadlock and Reach)
module Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkRange,
  checkPetriNetSizes,
  checkTransitionLengths,
  checkRejectLongerThanConsistency,
  checkCapacity,
  checkFilterConfigWith,
  checkMaxPrintedSolutions
) where

import Control.Applicative (Alternative ((<|>)))
import Data.GraphViz.Commands (GraphvizCommand)
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  noFiltering,
  )
import Modelling.PetriNet.Reach.Type (Capacity(..))

-- | Check that a range (low, high) is valid
checkRange
  :: (Num n, Ord n, Show n)
  => String          -- ^ Description of what is being checked
  -> (n, Maybe n)    -- ^ (lower bound, upper bound)
  -> Maybe String
checkRange what (low, h)
  | low < 0 = Just $ "The lower limit for " ++ what ++ " has to be at least 0!"
  | otherwise = case h of
      Nothing -> Nothing  -- No upper bound specified, only check lower bound
      Just high ->
        if high < low
        then Just $ "The upper limit (currently " ++ show h ++ "; second value) for " ++ what ++
                   " has to be at least as high as its lower limit (currently " ++ show low ++ "; first value)!"
        else Nothing

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
  | minTransitionLength > maxTransitionLength = Just $
    "minTransitionLength (" ++ show minTransitionLength ++ ") cannot be greater than maxTransitionLength (" ++ show maxTransitionLength ++ ")"
  | otherwise = Nothing

-- | Check that capacity is set to Unbounded
checkCapacity :: Capacity s -> Maybe String
checkCapacity Unbounded = Nothing
checkCapacity _ = Just "Other choices for 'capacity' than 'Unbounded' are not currently supported for this task type."

-- | Check consistency between rejectLongerThan and other length parameters
checkRejectLongerThanConsistency :: Maybe Int -> Int -> Bool -> Maybe String
checkRejectLongerThanConsistency rejectLongerThan maxTransitionLength showLengthHint =
  case rejectLongerThan of
    Just rejectLength
      | rejectLength <= 0 -> Just "rejectLongerThan must be positive when specified"
      | rejectLength < maxTransitionLength -> Just $
        "rejectLongerThan (" ++ show rejectLength ++ ") cannot be less than maxTransitionLength (" ++ show maxTransitionLength ++ ")"
      | rejectLength == maxTransitionLength && showLengthHint -> Just "showLengthHint == True does not make sense when rejectLongerThan equals maxTransitionLength"
      | otherwise -> Nothing
    Nothing -> Nothing

-- | Check basic Petri net configuration including sizes, lengths, ranges, capacity and draw commands
checkBasicPetriConfig
  :: Int                      -- ^ numPlaces
  -> Int                      -- ^ numTransitions
  -> Capacity s               -- ^ capacity
  -> Int                      -- ^ minTransitionLength
  -> Int                      -- ^ maxTransitionLength
  -> (Int, Maybe Int)         -- ^ preconditionsRange
  -> (Int, Maybe Int)         -- ^ postconditionsRange
  -> [GraphvizCommand]        -- ^ drawCommands
  -> Maybe Int                -- ^ rejectLongerThan
  -> Bool                     -- ^ showLengthHint
  -> Maybe String
checkBasicPetriConfig
  numPlaces
  numTransitions
  capacity
  minTransitionLength
  maxTransitionLength
  preconditionsRange
  postconditionsRange
  drawCommands
  rejectLongerThan
  showLengthHint =
    checkPetriNetSizes numPlaces numTransitions
    <|> checkCapacity capacity
    <|> checkTransitionLengths minTransitionLength maxTransitionLength
    <|> checkRange "preconditionsRange" preconditionsRange
    <|> checkRange "postconditionsRange" postconditionsRange
    <|> checkRejectLongerThanConsistency rejectLongerThan maxTransitionLength showLengthHint
    <|> checkDrawCommands drawCommands
  where
    checkDrawCommands [] = Just "drawCommands cannot be empty"
    checkDrawCommands _  = Nothing

-- | Check filter configuration constraints given the transition length parameters
checkFilterConfigWith
  :: Maybe Int        -- ^ rejectLongerThan
  -> Int              -- ^ minTransitionLength
  -> Int              -- ^ maxTransitionLength
  -> FilterConfig     -- ^ filterConfig
  -> Maybe String
checkFilterConfigWith rejectLongerThan minTransitionLength maxTransitionLength filterConfig@FilterConfig{..}
  | rejectLongerThan /= Just minTransitionLength
  , filterConfig /= noFiltering
  = Just $ "If transition length is not enforced to one value, filterConfig must be set to "
    ++ show noFiltering
  | Just repeats <- minRepetitiveLength
  , repeats < 2
  = Just "minRepetitiveLength has to be set to at least 2 if it is enabled"
  | Just repeats <- minRepetitiveLength
  , repeats > maxTransitionLength `div` 2
  = Just "minRepetitiveLength must not be higher than half of maxTransitionLength"
  | Just cycleLength <- maxCycleLength
  , cycleLength < 1
  = Just "setting maxCycleLength to less than 1 does not make sense"
  | Just cycleLength <- maxCycleLength
  , cycleLength > maxTransitionLength `div` 2
  = Just "maxCycleLength must not be higher than half of maxTransitionLength"
  | Just spaceballsLength <- minSpaceballsLength
  , spaceballsLength < 2 || spaceballsLength > maxTransitionLength
  = Just "minSpaceballsLength must be a value from 2 to maxTransitionLength if it is enabled"
  | Just maxSolutions <- maxNumberOfSolutions
  , maxSolutions < 1
  = Just "setting maxNumberOfSolutions to less than 1 does not make sense"
  | minTransitionCoverage < 0 || minTransitionCoverage > 1
  = Just "minTransitionCoverage must be a value from 0 to 1"
  | otherwise
  = Nothing

-- | Check maxPrintedSolutions is consistent with filterConfig
checkMaxPrintedSolutions
  :: Int             -- ^ maxPrintedSolutions
  -> FilterConfig    -- ^ filterConfig
  -> Maybe String
checkMaxPrintedSolutions maxPrintedSolutions FilterConfig{..}
  | maxPrintedSolutions < 0
  = Just "maxPrintedSolutions must be non-negative"
  | maxPrintedSolutions > 0
  , Just maxSolutions <- maxNumberOfSolutions
  , maxPrintedSolutions > maxSolutions
  = Just $ "maxPrintedSolutions (" ++ show maxPrintedSolutions ++ ") cannot be greater than maxNumberOfSolutions (" ++ show maxSolutions ++ ")"
  | otherwise
  = Nothing
