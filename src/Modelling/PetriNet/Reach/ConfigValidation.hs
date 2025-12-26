{-# LANGUAGE RecordWildCards #-}

-- | Common validation logic for Petri Net configurations (Deadlock and Reach)
module Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkRange,
  checkPetriNetSizes,
  checkTransitionLengths,
  checkRejectLongerThanConsistency,
  checkCapacity,
  checkMaxPlaceDifference,
  checkFilterConfigWith
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

-- | Check maxPlaceDifference is within valid bounds
checkMaxPlaceDifference :: Maybe Int -> Int -> Maybe String
checkMaxPlaceDifference maybeMaxPlaceDifference numPlaces =
  case maybeMaxPlaceDifference of
    Nothing -> Nothing
    Just maxPlaceDiff
      | maxPlaceDiff < 1 -> Just "maxPlaceDifference must be at least 1 when specified"
      | maxPlaceDiff > numPlaces -> Just $
        "maxPlaceDifference (" ++ show maxPlaceDiff ++ ") cannot be greater than numPlaces (" ++ show numPlaces ++ ")"
      | otherwise -> Nothing

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
  -> Maybe Int                -- ^ maxPlaceDifference
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
  showLengthHint
  maxPlaceDifference =
    checkPetriNetSizes numPlaces numTransitions
    <|> checkCapacity capacity
    <|> checkTransitionLengths minTransitionLength maxTransitionLength
    <|> checkRange "preconditionsRange" preconditionsRange
    <|> checkRange "postconditionsRange" postconditionsRange
    <|> checkRejectLongerThanConsistency rejectLongerThan maxTransitionLength showLengthHint
    <|> checkMaxPlaceDifference maxPlaceDifference numPlaces
    <|> checkDrawCommands drawCommands
  where
    checkDrawCommands [] = Just "drawCommands cannot be empty"
    checkDrawCommands _  = Nothing

-- | Check filter configuration constraints given the transition length parameters
checkFilterConfigWith
  :: Maybe Int        -- ^ rejectLongerThan
  -> Int              -- ^ minTransitionLength
  -> Int              -- ^ maxTransitionLength
  -> Int              -- ^ numTransitions (total number of transitions)
  -> FilterConfig     -- ^ filterConfig
  -> Maybe String
checkFilterConfigWith rejectLongerThan minTransitionLength maxTransitionLength numTransitions filterConfig@FilterConfig{..}
  | rejectLongerThan /= Just minTransitionLength
  , filterConfig /= noFiltering
  = Just $ "If transition length is not enforced to one value, filterConfig must be set to "
    ++ show noFiltering
  | Just repeats <- repetitiveSubsequenceThreshold
  , repeats < 2
  = Just "repetitiveSubsequenceThreshold has to be set to at least 2 if it is enabled"
  | Just repeats <- repetitiveSubsequenceThreshold
  , repeats > maxTransitionLength `div` 2
  = Just "repetitiveSubsequenceThreshold must not be higher than half of maxTransitionLength"
  | Just cycleLength <- rejectCyclesUpToLength
  , cycleLength < 1
  = Just "setting rejectCyclesUpToLength to less than 1 does not make sense"
  | Just cycleLength <- rejectCyclesUpToLength
  , cycleLength > maxTransitionLength `div` 2
  = Just "rejectCyclesUpToLength must not be higher than half of maxTransitionLength"
  | Just spaceballsLength <- spaceballsPrefixThreshold
  , spaceballsLength < 2 || spaceballsLength > maxTransitionLength
  = Just "spaceballsPrefixThreshold must be a value from 2 to maxTransitionLength if it is enabled"
  | Just maxSolutions <- solutionSetLimit
  , maxSolutions < 1
  = Just "setting solutionSetLimit to less than 1 does not make sense"
  | solutionSetLimit == Just 1
  , not requireSolutionsArePermutations
  = Just "when solutionSetLimit is 1, requireSolutionsArePermutations might as well be set to True"
  | transitionCoverageRequirement < 0 || transitionCoverageRequirement > 1
  = Just "transitionCoverageRequirement must be a value from 0 to 1"
  | absentTransitionsRequirement < 0 || absentTransitionsRequirement >= numTransitions
  = Just "absentTransitionsRequirement must be non-negative and smaller than the total number of transitions"
  | let maxAbsent = floor ((1 - transitionCoverageRequirement) * fromIntegral numTransitions)
  , absentTransitionsRequirement > maxAbsent
  = Just $ "absentTransitionsRequirement conflicts with transitionCoverageRequirement: " ++
           "at most " ++ show maxAbsent ++ " transitions can be absent given the coverage requirement"
  | otherwise
  = Nothing
