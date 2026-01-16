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
import Modelling.PetriNet.Reach.Type (Capacity(..), TransitionBehaviorConstraints(..), ArrowDensityConstraints(..))

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
  -> TransitionBehaviorConstraints -- ^ transitionBehaviorConstraints
  -> ArrowDensityConstraints  -- ^ arrowDensityConstraints
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
  transitionBehaviorConstraints
  arrowDensityConstraints
  drawCommands
  rejectLongerThan
  showLengthHint =
    checkPetriNetSizes numPlaces numTransitions
    <|> checkCapacity capacity
    <|> checkTransitionLengths minTransitionLength maxTransitionLength
    <|> checkTransitionBehaviorConstraints
          numPlaces
          numTransitions
          arrowDensityConstraints
          transitionBehaviorConstraints
    <|> checkRange "incomingArrowsPerTransition" (incomingArrowsPerTransition arrowDensityConstraints)
    <|> checkRange "outgoingArrowsPerTransition" (outgoingArrowsPerTransition arrowDensityConstraints)
    <|> checkRange "incomingArrowsPerPlace" (incomingArrowsPerPlace arrowDensityConstraints)
    <|> checkRange "outgoingArrowsPerPlace" (outgoingArrowsPerPlace arrowDensityConstraints)
    <|> checkRange "totalArrowsFromPlacesToTransitions" (totalArrowsFromPlacesToTransitions arrowDensityConstraints)
    <|> checkRange "totalArrowsFromTransitionsToPlaces" (totalArrowsFromTransitionsToPlaces arrowDensityConstraints)
    <|> checkRangeVersusCount "numPlaces" numPlaces "incomingArrowsPerTransition"
          (incomingArrowsPerTransition arrowDensityConstraints)
    <|> checkRangeVersusCount "numPlaces" numPlaces "outgoingArrowsPerTransition"
          (outgoingArrowsPerTransition arrowDensityConstraints)
    <|> checkRangeVersusCount "numTransitions" numTransitions "incomingArrowsPerPlace"
          (incomingArrowsPerPlace arrowDensityConstraints)
    <|> checkRangeVersusCount "numTransitions" numTransitions "outgoingArrowsPerPlace"
          (outgoingArrowsPerPlace arrowDensityConstraints)
    <|> checkRangeVersusCount "numPlaces * numTransitions" (numPlaces * numTransitions)
          "totalArrowsFromPlacesToTransitions"
          (totalArrowsFromPlacesToTransitions arrowDensityConstraints)
    <|> checkRangeVersusCount "numPlaces * numTransitions" (numPlaces * numTransitions)
          "totalArrowsFromTransitionsToPlaces"
          (totalArrowsFromTransitionsToPlaces arrowDensityConstraints)
    <|> checkRejectLongerThanConsistency rejectLongerThan maxTransitionLength showLengthHint
    <|> checkDrawCommands drawCommands
    <|> checkArrowDensityCrossValidation numPlaces numTransitions arrowDensityConstraints
  where
    checkDrawCommands [] = Just "drawCommands cannot be empty"
    checkDrawCommands _  = Nothing
    checkRangeVersusCount countName count what (low, h) = case h of
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
  -> Int                               -- ^ numTransitions
  -> ArrowDensityConstraints           -- ^ arrow density constraints
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> Maybe String
checkTransitionBehaviorConstraints
  numPlaces
  numTransitions
  ArrowDensityConstraints {..}
  TransitionBehaviorConstraints {..}
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
  | Just direction <- allowedTokenChanges
  = let
      -- Use fromMaybe to handle both Just and Nothing cases
      -- For lower bound checks: at least 0 non-preserving transitions
      minNonPreserving = fromMaybe 0 areNonPreserving
      -- For upper bound checks: at most all transitions are non-preserving
      maxNonPreserving = fromMaybe numTransitions areNonPreserving

      -- Calculate differences and limits based on direction
      (lowerDiff, maxPerTransition) = case direction of
        GT -> (tnLow - tvLow, nHigh - vLow)
        LT -> (tvLow - tnLow, vHigh - nLow)

      maxTotal = maxNonPreserving * maxPerTransition

      -- Check lower bound insufficient arrow difference
      checkLowerInsufficientDiff
        | lowerDiff < minNonPreserving
        = Just $ insufficientArrowDifference direction minNonPreserving "lower" tvLow tnLow
        | otherwise
        = Nothing

      -- Check lower bound excessive arrow difference
      checkLowerExcessiveDiff
        | lowerDiff > maxTotal
        = Just $ excessiveArrowDifference direction maxNonPreserving maxPerTransition maxTotal "lower" tvLow tnLow lowerDiff
        | otherwise
        = Nothing

      -- Check upper bound insufficient arrow difference
      checkUpperInsufficientDiff = case direction of
        GT | Just tvHighValue <- tvHighMaybe
           , tnHigh - tvHighValue < minNonPreserving
           -> Just $ insufficientArrowDifference direction minNonPreserving "upper" tvHighValue tnHigh
        LT | Just tnHighValue <- tnHighMaybe
           , tvHigh - tnHighValue < minNonPreserving
           -> Just $ insufficientArrowDifference direction minNonPreserving "upper" tvHigh tnHighValue
        _ -> Nothing

      -- Check upper bound excessive arrow difference
      checkUpperExcessiveDiff = case direction of
        GT | Just tnHighValue <- tnHighMaybe
           , tnHighValue - tvHigh > maxTotal
           -> Just $ excessiveArrowDifference direction maxNonPreserving maxPerTransition maxTotal "upper" tvHigh tnHighValue (tnHighValue - tvHigh)
        LT | Just tvHighValue <- tvHighMaybe
           , tvHighValue - tnHigh > maxTotal
           -> Just $ excessiveArrowDifference direction maxNonPreserving maxPerTransition maxTotal "upper" tvHighValue tnHigh (tvHighValue - tnHigh)
        _ -> Nothing

    in checkLowerInsufficientDiff <|> checkLowerExcessiveDiff <|> checkUpperInsufficientDiff <|> checkUpperExcessiveDiff
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
    (tvLow, tvHighMaybe) = totalArrowsFromPlacesToTransitions
    (tnLow, tnHighMaybe) = totalArrowsFromTransitionsToPlaces
    -- For total arrows, use the product of numPlaces * numTransitions as the default upper bound.
    -- This represents the maximum possible number of arrow connections (not counting weights):
    -- each place can have at most one arrow connection to each transition.
    tvHigh = fromMaybe (numPlaces * numTransitions) tvHighMaybe
    tnHigh = fromMaybe (numPlaces * numTransitions) tnHighMaybe

    -- Helper function for insufficient arrow difference errors
    insufficientArrowDifference direction numberOfNonPreserving boundType placeValue transitionValue = unwords
      [ "with allowedTokenChanges = Just"
      , show direction
      , "and areNonPreserving = Just"
      , show numberOfNonPreserving ++ ","
      , boundType
      , "bound difference between totalArrowsFromPlacesToTransitions (" ++ show placeValue ++ ")"
      , "and totalArrowsFromTransitionsToPlaces (" ++ show transitionValue ++ ")"
      , "must be at least"
      , show numberOfNonPreserving
      ]

    -- Helper function for excessive arrow difference errors
    excessiveArrowDifference direction numberOfNonPreserving maxPerTransition maxTotalTokenChange boundType placeValue transitionValue actualDifference = unwords
      [ "with allowedTokenChanges = Just"
      , show direction
      , "and areNonPreserving = Just"
      , show numberOfNonPreserving ++ ","
      , "at most"
      , show maxPerTransition
      , "token"
      , if direction == GT then "increase" else "decrease"
      , "per transition is possible,"
      , "so overall at most"
      , show maxTotalTokenChange
      , "token"
      , if direction == GT then "increase" else "decrease"
      , "is possible,"
      , "but"
      , boundType
      , "bound difference between totalArrowsFromPlacesToTransitions (" ++ show placeValue ++ ")"
      , "and totalArrowsFromTransitionsToPlaces (" ++ show transitionValue ++ ")"
      , "is"
      , show actualDifference
      ]

-- | Check cross-validation of arrow density parameters
checkArrowDensityCrossValidation
  :: Int              -- ^ numPlaces
  -> Int              -- ^ numTransitions
  -> ArrowDensityConstraints
  -> Maybe String
checkArrowDensityCrossValidation
  numPlaces
  numTransitions
  ArrowDensityConstraints {
    incomingArrowsPerTransition = (incomingPerTransLow, incomingPerTransHigh),
    outgoingArrowsPerTransition = (outgoingPerTransLow, outgoingPerTransHigh),
    incomingArrowsPerPlace = (incomingPerPlaceLow, incomingPerPlaceHigh),
    outgoingArrowsPerPlace = (outgoingPerPlaceLow, outgoingPerPlaceHigh),
    totalArrowsFromPlacesToTransitions = (totalPlacesToTransLow, totalPlacesToTransHigh),
    totalArrowsFromTransitionsToPlaces = (totalTransToPlacesLow, totalTransToPlacesHigh)
  }
  | let minPlacesToTrans = max (incomingPerTransLow * numTransitions) (outgoingPerPlaceLow * numPlaces)
  , totalPlacesToTransLow < minPlacesToTrans
  = Just $ "given the other settings, totalArrowsFromPlacesToTransitions lower bound (" ++ show totalPlacesToTransLow ++
           ") should be at least " ++ show minPlacesToTrans
  | let maxPlacesToTrans = min (incomingPerTransHighBound * numTransitions) (outgoingPerPlaceHighBound * numPlaces)
  , let bound = fromMaybe totalPlacesToTransLow totalPlacesToTransHigh
  , bound > maxPlacesToTrans
  = Just $ "given the other settings, totalArrowsFromPlacesToTransitions (upper) bound (" ++
           show bound ++
           ") should be at most " ++ show maxPlacesToTrans
  | let minTransToPlaces = max (outgoingPerTransLow * numTransitions) (incomingPerPlaceLow * numPlaces)
  , totalTransToPlacesLow < minTransToPlaces
  = Just $ "given the other settings, totalArrowsFromTransitionsToPlaces lower bound (" ++ show totalTransToPlacesLow ++
           ") should be at least " ++ show minTransToPlaces
  | let maxTransToPlaces = min (outgoingPerTransHighBound * numTransitions) (incomingPerPlaceHighBound * numPlaces)
  , let bound = fromMaybe totalTransToPlacesLow totalTransToPlacesHigh
  , bound > maxTransToPlaces
  = Just $ "given the other settings, totalArrowsFromTransitionsToPlaces (upper) bound (" ++
           show bound ++
           ") should be at most " ++ show maxTransToPlaces
  | incomingPerTransLow * numTransitions > outgoingPerPlaceHighBound * numPlaces
  = Just $ "incomingArrowsPerTransition lower bound times numTransitions " ++
           "exceeds maximum possible arrows based on outgoingArrowsPerPlace (or numTransitions) times numPlaces"
  | outgoingPerTransLow * numTransitions > incomingPerPlaceHighBound * numPlaces
  = Just $ "outgoingArrowsPerTransition lower bound times numTransitions " ++
           "exceeds maximum possible arrows based on incomingArrowsPerPlace (or numTransitions) times numPlaces"
  | otherwise = Nothing
  where
    incomingPerTransHighBound = fromMaybe numPlaces incomingPerTransHigh
    outgoingPerTransHighBound = fromMaybe numPlaces outgoingPerTransHigh
    incomingPerPlaceHighBound = fromMaybe numTransitions incomingPerPlaceHigh
    outgoingPerPlaceHighBound = fromMaybe numTransitions outgoingPerPlaceHigh
