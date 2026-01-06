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
  checkFilterConfigWith,
  checkArrowDensityCrossValidation
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
  -> (Int, Maybe Int)         -- ^ incomingArrowsPerTransition
  -> (Int, Maybe Int)         -- ^ outgoingArrowsPerTransition
  -> (Int, Maybe Int)         -- ^ incomingArrowsPerPlace
  -> (Int, Maybe Int)         -- ^ outgoingArrowsPerPlace
  -> (Int, Maybe Int)         -- ^ totalArrowsFromPlacesToTransitions
  -> (Int, Maybe Int)         -- ^ totalArrowsFromTransitionsToPlaces
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
  incomingArrowsPerTransition
  outgoingArrowsPerTransition
  incomingArrowsPerPlace
  outgoingArrowsPerPlace
  totalArrowsFromPlacesToTransitions
  totalArrowsFromTransitionsToPlaces
  drawCommands
  rejectLongerThan
  showLengthHint =
    checkPetriNetSizes numPlaces numTransitions
    <|> checkCapacity capacity
    <|> checkTransitionLengths minTransitionLength maxTransitionLength
    <|> checkRange "incomingArrowsPerTransition" incomingArrowsPerTransition
    <|> checkRange "outgoingArrowsPerTransition" outgoingArrowsPerTransition
    <|> checkRange "incomingArrowsPerPlace" incomingArrowsPerPlace
    <|> checkRange "outgoingArrowsPerPlace" outgoingArrowsPerPlace
    <|> checkRange "totalArrowsFromPlacesToTransitions" totalArrowsFromPlacesToTransitions
    <|> checkRange "totalArrowsFromTransitionsToPlaces" totalArrowsFromTransitionsToPlaces
    <|> checkRangeVersusPlaces "incomingArrowsPerTransition" incomingArrowsPerTransition numPlaces
    <|> checkRangeVersusPlaces "outgoingArrowsPerTransition" outgoingArrowsPerTransition numPlaces
    <|> checkRangeVersusTransitions "incomingArrowsPerPlace" incomingArrowsPerPlace numTransitions
    <|> checkRangeVersusTransitions "outgoingArrowsPerPlace" outgoingArrowsPerPlace numTransitions
    <|> checkRejectLongerThanConsistency rejectLongerThan maxTransitionLength showLengthHint
    <|> checkDrawCommands drawCommands
    <|> checkArrowDensityCrossValidation
          numPlaces
          numTransitions
          incomingArrowsPerTransition
          outgoingArrowsPerTransition
          incomingArrowsPerPlace
          outgoingArrowsPerPlace
          totalArrowsFromPlacesToTransitions
          totalArrowsFromTransitionsToPlaces
  where
    checkDrawCommands [] = Just "drawCommands cannot be empty"
    checkDrawCommands _  = Nothing
    checkRangeVersusPlaces = checkRangeVersusCount "numPlaces"
    checkRangeVersusTransitions = checkRangeVersusCount "numTransitions"
    checkRangeVersusCount countName what (low, h) count = case h of
      Nothing ->
        if low > count
        then Just $ "The lower limit for " ++ what ++ " (currently " ++ show low ++
                   ") cannot exceed " ++ countName ++ " (currently " ++ show count ++ ")"
        else Nothing
      Just high ->
        if high > count
        then Just $ "The upper limit for " ++ what ++ " (currently " ++ show high ++
                   ") cannot exceed " ++ countName ++ " (currently " ++ show count ++ ")"
        else Nothing

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
  :: Int                               -- ^ numPlaces
  -> (Int, Maybe Int)                  -- ^ incomingArrowsPerTransition
  -> (Int, Maybe Int)                  -- ^ outgoingArrowsPerTransition
  -> Int                               -- ^ numTransitions
  -> TransitionBehaviorConstraints     -- ^ constraints
  -> Maybe String
checkTransitionBehaviorConstraints numPlaces incomingArrowsPerTransition outgoingArrowsPerTransition numTransitions TransitionBehaviorConstraints {..}
  | Just EQ <- allowedTokenChanges
  = Just "allowedTokenChanges = Just EQ is meaningless; use areNonPreserving = Just 0 instead"
  | Just numberOfNonPreserving <- areNonPreserving
  , numberOfNonPreserving < 0 || numberOfNonPreserving > numTransitions
  = Just "areNonPreserving must be non-negative and at most numTransitions when specified"
  | areNonPreserving == Just 0
  , isJust allowedTokenChanges
  = Just "when areNonPreserving = Just 0 (all transitions token-preserving), allowedTokenChanges = Just ... makes no sense"
  | allowedTokenChanges == Just LT
  , vLow < nLow || vHigh < nHigh
  = Just "with allowedTokenChanges = Just LT, the combination of incomingArrowsPerTransition and outgoingArrowsPerTransition is too lax"
  | allowedTokenChanges == Just GT
  , vLow > nLow || vHigh > nHigh
  = Just "with allowedTokenChanges = Just GT, the combination of incomingArrowsPerTransition and outgoingArrowsPerTransition is too lax"
  | areNonPreserving /= Just 0
  , vLow == vHigh && nLow == nHigh && vLow == nLow
  = Just "only areNonPreserving = Just 0 makes sense when incomingArrowsPerTransition and outgoingArrowsPerTransition are all fixed to one value anyway"
  | otherwise
  = Nothing
  where
    (vLow, vHighMaybe) = incomingArrowsPerTransition
    (nLow, nHighMaybe) = outgoingArrowsPerTransition
    -- Since checkBasicPetriConfig guarantees upper bounds don't exceed numPlaces, we can use numPlaces as the default
    vHigh = fromMaybe numPlaces vHighMaybe
    nHigh = fromMaybe numPlaces nHighMaybe

-- | Check cross-validation of arrow density parameters
checkArrowDensityCrossValidation
  :: Int              -- ^ numPlaces
  -> Int              -- ^ numTransitions
  -> (Int, Maybe Int) -- ^ incomingArrowsPerTransition
  -> (Int, Maybe Int) -- ^ outgoingArrowsPerTransition
  -> (Int, Maybe Int) -- ^ incomingArrowsPerPlace
  -> (Int, Maybe Int) -- ^ outgoingArrowsPerPlace
  -> (Int, Maybe Int) -- ^ totalArrowsFromPlacesToTransitions
  -> (Int, Maybe Int) -- ^ totalArrowsFromTransitionsToPlaces
  -> Maybe String
checkArrowDensityCrossValidation
  numPlaces
  numTransitions
  (incomingPerTransLow, incomingPerTransHigh)
  (outgoingPerTransLow, outgoingPerTransHigh)
  (incomingPerPlaceLow, incomingPerPlaceHigh)
  (outgoingPerPlaceLow, outgoingPerPlaceHigh)
  (totalPlacesToTransLow, totalPlacesToTransHigh)
  (totalTransToPlacesLow, totalTransToPlacesHigh)
  -- totalArrowsFromPlacesToTransitions relates to incomingArrowsPerTransition
  -- Check that totalArrowsFromPlacesToTransitions is consistent with per-transition bounds
  | totalPlacesToTransLow > incomingPerTransHighBound * numTransitions
  = Just $ "totalArrowsFromPlacesToTransitions lower bound (" ++ show totalPlacesToTransLow ++
           ") exceeds maximum possible arrows based on incomingArrowsPerTransition (" ++
           show (incomingPerTransHighBound * numTransitions) ++ ")"
  | Just totalHigh <- totalPlacesToTransHigh
  , totalHigh < incomingPerTransLow * numTransitions
  = Just $ "totalArrowsFromPlacesToTransitions upper bound (" ++ show totalHigh ++
           ") is less than minimum required arrows based on incomingArrowsPerTransition (" ++
           show (incomingPerTransLow * numTransitions) ++ ")"
  -- Aggressive narrowing: if per-transition lower bound implies higher total, require it
  | totalPlacesToTransLow < incomingPerTransLow * numTransitions
  = Just $ "totalArrowsFromPlacesToTransitions lower bound (" ++ show totalPlacesToTransLow ++
           ") should be at least " ++ show (incomingPerTransLow * numTransitions) ++
           " to match incomingArrowsPerTransition lower bound of " ++ show incomingPerTransLow ++
           " per transition across " ++ show numTransitions ++ " transitions"
  -- Aggressive narrowing: if per-transition upper bound implies tighter total, require it
  | Just totalHigh <- totalPlacesToTransHigh
  , totalHigh > incomingPerTransHighBound * numTransitions
  = Just $ "totalArrowsFromPlacesToTransitions upper bound (" ++ show totalHigh ++
           ") should be at most " ++ show (incomingPerTransHighBound * numTransitions) ++
           " to match incomingArrowsPerTransition upper bound of " ++ show incomingPerTransHighBound ++
           " per transition across " ++ show numTransitions ++ " transitions"
  -- totalArrowsFromTransitionsToPlaces relates to outgoingArrowsPerTransition
  -- Check that totalArrowsFromTransitionsToPlaces is consistent with per-transition bounds
  | totalTransToPlacesLow > outgoingPerTransHighBound * numTransitions
  = Just $ "totalArrowsFromTransitionsToPlaces lower bound (" ++ show totalTransToPlacesLow ++
           ") exceeds maximum possible arrows based on outgoingArrowsPerTransition (" ++
           show (outgoingPerTransHighBound * numTransitions) ++ ")"
  | Just totalHigh <- totalTransToPlacesHigh
  , totalHigh < outgoingPerTransLow * numTransitions
  = Just $ "totalArrowsFromTransitionsToPlaces upper bound (" ++ show totalHigh ++
           ") is less than minimum required arrows based on outgoingArrowsPerTransition (" ++
           show (outgoingPerTransLow * numTransitions) ++ ")"
  -- Aggressive narrowing: if per-transition lower bound implies higher total, require it
  | totalTransToPlacesLow < outgoingPerTransLow * numTransitions
  = Just $ "totalArrowsFromTransitionsToPlaces lower bound (" ++ show totalTransToPlacesLow ++
           ") should be at least " ++ show (outgoingPerTransLow * numTransitions) ++
           " to match outgoingArrowsPerTransition lower bound of " ++ show outgoingPerTransLow ++
           " per transition across " ++ show numTransitions ++ " transitions"
  -- Aggressive narrowing: if per-transition upper bound implies tighter total, require it
  | Just totalHigh <- totalTransToPlacesHigh
  , totalHigh > outgoingPerTransHighBound * numTransitions
  = Just $ "totalArrowsFromTransitionsToPlaces upper bound (" ++ show totalHigh ++
           ") should be at most " ++ show (outgoingPerTransHighBound * numTransitions) ++
           " to match outgoingArrowsPerTransition upper bound of " ++ show outgoingPerTransHighBound ++
           " per transition across " ++ show numTransitions ++ " transitions"
  -- totalArrowsFromPlacesToTransitions relates to outgoingArrowsPerPlace
  -- Check that totalArrowsFromPlacesToTransitions is consistent with per-place bounds
  | totalPlacesToTransLow > outgoingPerPlaceHighBound * numPlaces
  = Just $ "totalArrowsFromPlacesToTransitions lower bound (" ++ show totalPlacesToTransLow ++
           ") exceeds maximum possible arrows based on outgoingArrowsPerPlace (" ++
           show (outgoingPerPlaceHighBound * numPlaces) ++ ")"
  | Just totalHigh <- totalPlacesToTransHigh
  , totalHigh < outgoingPerPlaceLow * numPlaces
  = Just $ "totalArrowsFromPlacesToTransitions upper bound (" ++ show totalHigh ++
           ") is less than minimum required arrows based on outgoingArrowsPerPlace (" ++
           show (outgoingPerPlaceLow * numPlaces) ++ ")"
  -- Aggressive narrowing: if per-place lower bound implies higher total, require it
  | totalPlacesToTransLow < outgoingPerPlaceLow * numPlaces
  = Just $ "totalArrowsFromPlacesToTransitions lower bound (" ++ show totalPlacesToTransLow ++
           ") should be at least " ++ show (outgoingPerPlaceLow * numPlaces) ++
           " to match outgoingArrowsPerPlace lower bound of " ++ show outgoingPerPlaceLow ++
           " per place across " ++ show numPlaces ++ " places"
  -- Aggressive narrowing: if per-place upper bound implies tighter total, require it
  | Just totalHigh <- totalPlacesToTransHigh
  , totalHigh > outgoingPerPlaceHighBound * numPlaces
  = Just $ "totalArrowsFromPlacesToTransitions upper bound (" ++ show totalHigh ++
           ") should be at most " ++ show (outgoingPerPlaceHighBound * numPlaces) ++
           " to match outgoingArrowsPerPlace upper bound of " ++ show outgoingPerPlaceHighBound ++
           " per place across " ++ show numPlaces ++ " places"
  -- totalArrowsFromTransitionsToPlaces relates to incomingArrowsPerPlace
  -- Check that totalArrowsFromTransitionsToPlaces is consistent with per-place bounds
  | totalTransToPlacesLow > incomingPerPlaceHighBound * numPlaces
  = Just $ "totalArrowsFromTransitionsToPlaces lower bound (" ++ show totalTransToPlacesLow ++
           ") exceeds maximum possible arrows based on incomingArrowsPerPlace (" ++
           show (incomingPerPlaceHighBound * numPlaces) ++ ")"
  | Just totalHigh <- totalTransToPlacesHigh
  , totalHigh < incomingPerPlaceLow * numPlaces
  = Just $ "totalArrowsFromTransitionsToPlaces upper bound (" ++ show totalHigh ++
           ") is less than minimum required arrows based on incomingArrowsPerPlace (" ++
           show (incomingPerPlaceLow * numPlaces) ++ ")"
  -- Aggressive narrowing: if per-place lower bound implies higher total, require it
  | totalTransToPlacesLow < incomingPerPlaceLow * numPlaces
  = Just $ "totalArrowsFromTransitionsToPlaces lower bound (" ++ show totalTransToPlacesLow ++
           ") should be at least " ++ show (incomingPerPlaceLow * numPlaces) ++
           " to match incomingArrowsPerPlace lower bound of " ++ show incomingPerPlaceLow ++
           " per place across " ++ show numPlaces ++ " places"
  -- Aggressive narrowing: if per-place upper bound implies tighter total, require it
  | Just totalHigh <- totalTransToPlacesHigh
  , totalHigh > incomingPerPlaceHighBound * numPlaces
  = Just $ "totalArrowsFromTransitionsToPlaces upper bound (" ++ show totalHigh ++
           ") should be at most " ++ show (incomingPerPlaceHighBound * numPlaces) ++
           " to match incomingArrowsPerPlace upper bound of " ++ show incomingPerPlaceHighBound ++
           " per place across " ++ show numPlaces ++ " places"
  -- Check that per-transition and per-place bounds are mutually consistent
  -- incomingArrowsPerTransition and outgoingArrowsPerPlace refer to the same arrows (places to transitions)
  | incomingPerTransLow * numTransitions > outgoingPerPlaceHighBound * numPlaces
  = Just $ "incomingArrowsPerTransition lower bound times numTransitions (" ++
           show (incomingPerTransLow * numTransitions) ++
           ") exceeds maximum possible arrows based on outgoingArrowsPerPlace (" ++
           show (outgoingPerPlaceHighBound * numPlaces) ++ ")"
  -- outgoingArrowsPerTransition and incomingArrowsPerPlace refer to the same arrows (transitions to places)
  | outgoingPerTransLow * numTransitions > incomingPerPlaceHighBound * numPlaces
  = Just $ "outgoingArrowsPerTransition lower bound times numTransitions (" ++
           show (outgoingPerTransLow * numTransitions) ++
           ") exceeds maximum possible arrows based on incomingArrowsPerPlace (" ++
           show (incomingPerPlaceHighBound * numPlaces) ++ ")"
  | otherwise = Nothing
  where
    incomingPerTransHighBound = fromMaybe numPlaces incomingPerTransHigh
    outgoingPerTransHighBound = fromMaybe numPlaces outgoingPerTransHigh
    incomingPerPlaceHighBound = fromMaybe numTransitions incomingPerPlaceHigh
    outgoingPerPlaceHighBound = fromMaybe numTransitions outgoingPerPlaceHigh
