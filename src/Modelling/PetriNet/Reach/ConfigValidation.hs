{-# LANGUAGE RecordWildCards #-}

-- | Common validation logic for Petri Net configurations (Deadlock and Reach)
module Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkRange,
  checkPetriNetSizes,
  checkTransitionLengths,
  checkRejectLongerThanConsistency,
  checkCapacity,
  checkTransitionBehaviorConstraints,
  checkFilterConfigWith
) where

import Control.Applicative (Alternative ((<|>)))
import Data.GraphViz.Commands (GraphvizCommand)
import Data.List.Extra (notNull)
import Data.Maybe (fromMaybe, isJust)
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  noFiltering,
  )
import Modelling.PetriNet.Reach.Type (Capacity(..), TransitionBehaviorConstraints(..))

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
  -> Int              -- ^ numTransitions (total number of transitions)
  -> FilterConfig     -- ^ filterConfig
  -> Maybe String
checkFilterConfigWith rejectLongerThan theTransitionLength@minTransitionLength numTransitions filterConfig@FilterConfig{..}
  | rejectLongerThan /= Just minTransitionLength
  , filterConfig /= noFiltering
  = Just $ "If transition length is not enforced to one value, filterConfig must be set to "
    ++ show noFiltering
  | Just repeats <- repetitiveSubsequenceThreshold
  , repeats < 2
  = Just "repetitiveSubsequenceThreshold has to be set to at least 2 if it is enabled"
  | Just repeats <- repetitiveSubsequenceThreshold
  , repeats > halfTransitionLength
  = Just "repetitiveSubsequenceThreshold must not be higher than half of maxTransitionLength"
  | not (isSorted forbiddenCycleLengths) || not (isSorted requireCycleLengthsAny)
  = Just "forbiddenCycleLengths and requireCycleLengthsAny must each be sorted in ascending order"
  | notNull forbiddenCycleLengths && head forbiddenCycleLengths < 2
  = Just "forbiddenCycleLengths must contain only values greater than 1"
  | notNull requireCycleLengthsAny && head requireCycleLengthsAny < 1
  = Just "requireCycleLengthsAny must contain only positive values"
  | notNull forbiddenCycleLengths && last forbiddenCycleLengths > halfTransitionLength
  = Just "forbiddenCycleLengths must not contain values higher than half of maxTransitionLength"
  | notNull requireCycleLengthsAny && last requireCycleLengthsAny > halfTransitionLength
  = Just "requireCycleLengthsAny must not contain values higher than half of maxTransitionLength"
  | any ((0 /=) . mod theTransitionLength) (forbiddenCycleLengths ++ requireCycleLengthsAny)
  = Just "forbiddenCycleLengths and requireCycleLengthsAny must each contain only divisors of the target sequence length"
  | any (< minRequiredTransitions) (forbiddenCycleLengths ++ requireCycleLengthsAny)
  = Just "forbiddenCycleLengths or requireCycleLengthsAny contains values that are already impossible due to transitionCoverageRequirement"
  | hasRedundantMultiples forbiddenCycleLengths
  = Just "forbiddenCycleLengths contains redundant multiples (no need to forbid n if k*n for some k>1 is already forbidden)"
  | hasRedundantMultiples requireCycleLengthsAny
  = Just "requireCycleLengthsAny contains redundant multiples (no need to ask e.g. for 'n or 2*n', since asking for '2*n' would suffice)"
  | hasConflictBetweenForbiddenAndRequired forbiddenCycleLengths requireCycleLengthsAny
  = Just "requireCycleLengthsAny and forbiddenCycleLengths must not have overlapping or conflicting values"
  | 1 `elem` requireCycleLengthsAny && isJust repetitiveSubsequenceThreshold
  = Just "if requireCycleLengthsAny contains 1, repetitiveSubsequenceThreshold should be Nothing \
         \(forbidding repetitive subsequences does not make sense when requiring cycle length 1)"
  | Just spaceballsLength <- spaceballsPrefixThreshold
  , spaceballsLength < 2 || spaceballsLength > theTransitionLength
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
  | absentTransitionsRequirement > maxAbsent
  = Just $ "absentTransitionsRequirement conflicts with transitionCoverageRequirement: " ++
           "at most " ++ show maxAbsent ++ " transitions can be absent given the coverage requirement"
  | otherwise
  = Nothing
  where
    halfTransitionLength = theTransitionLength `div` 2
    minRequiredTransitions = ceiling (transitionCoverageRequirement * fromIntegral numTransitions)
    maxAbsent = numTransitions - minRequiredTransitions

    isSorted :: Ord a => [a] -> Bool
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:rest@(y:_)) = x < y && isSorted rest

    hasRedundantMultiples :: [Int] -> Bool
    hasRedundantMultiples = go []
      where
        go :: [Int] -> [Int] -> Bool
        go _ [] = False
        go smallerElements (currentElement : remainingElements)
          | any ((0 ==) . mod currentElement) smallerElements = True
          | otherwise = go (currentElement : smallerElements) remainingElements

    hasConflictBetweenForbiddenAndRequired :: [Int] -> [Int] -> Bool
    hasConflictBetweenForbiddenAndRequired forbidden =
      any (\r -> any (\f -> f `mod` r == 0) forbidden)

-- | Check transition behavior constraints for validity
checkTransitionBehaviorConstraints
  :: (Int, Maybe Int)                  -- ^ preconditionsRange
  -> (Int, Maybe Int)                  -- ^ postconditionsRange
  -> Int                               -- ^ numTransitions
  -> TransitionBehaviorConstraints     -- ^ constraints
  -> Maybe String
checkTransitionBehaviorConstraints preconditionsRange postconditionsRange numTransitions TransitionBehaviorConstraints {..}
  | Just EQ <- allowedTokenChanges
  = Just "allowedTokenChanges = Just EQ is meaningless; use areNonPreserving = Just 0 instead"
  | Just numberOfNonPreserving <- areNonPreserving
  , numberOfNonPreserving < 0 || numberOfNonPreserving > numTransitions
  = Just "areNonPreserving must be non-negative and not greater than numTransitions when specified"
  | areNonPreserving == Just 0
  , isJust allowedTokenChanges
  = Just "When areNonPreserving = 0 (all transitions token-preserving), allowedTokenChanges must be Nothing"
  | Just LT <- allowedTokenChanges
  , vHigh <= nLow
  = Just "allowedTokenChanges = Just LT (only token-decreasing) is impossible with the given ranges: \
         \all transitions would have consumed <= produced"
  | Just GT <- allowedTokenChanges
  , nHigh <= vLow
  = Just "allowedTokenChanges = Just GT (only token-increasing) is impossible with the given ranges: \
         \all transitions would have produced <= consumed"
  | Just numberOfNonPreserving <- areNonPreserving
  , numberOfNonPreserving > 0
  , vLow == vHigh && nLow == nHigh && vLow == nLow
  = Just $ "areNonPreserving = " ++ show numberOfNonPreserving ++ " is impossible: \
           \with preconditionsRange and postconditionsRange both fixed at " ++ show vLow ++ ", \
           \all transitions are token-preserving"
  | otherwise
  = Nothing
  where
    (vLow, vHighMaybe) = preconditionsRange
    (nLow, nHighMaybe) = postconditionsRange
    vHigh = fromMaybe maxBound vHighMaybe
    nHigh = fromMaybe maxBound nHighMaybe
