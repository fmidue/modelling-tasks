{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,18,0)
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
#endif
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Deadlock.hs
-}
module Modelling.PetriNet.Reach.Deadlock (
  -- * Types
  DeadlockInstance(..),
  DeadlockConfig(..),
  checkDeadlockConfig,

  -- * Generation
  generateDeadlock,

  -- * Task creation
  deadlockTask,
  verifyDeadlock,

  -- * Evaluation
  deadlockEvaluation,
  deadlockSyntax,
  deadlockInitial,

  -- * Configuration
  defaultDeadlockConfig,
  defaultDeadlockInstance,

  -- * Utilities
  bimapDeadlockInstance,
  toShowDeadlockInstance,
  exampleInstance,
) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

import Data.List.NonEmpty                 (NonEmpty((:|)))

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  defaultFilterConfig,
  )
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkFilterConfigWith,
  )
import Modelling.PetriNet.Reach.Reach   (
  assertReachPoints,
  isNoLonger,
  levelsWithAlternatives,
  rejectSpaceballsPattern,
  reportReachFor,
  transitionsValid,
  provideSolutionsFeedback,
  validateDrawabilityAndSolutionFiltering,
  )
import Modelling.PetriNet.Reach.Roll    (netLimitsFiltered)
import Modelling.PetriNet.Reach.Step    (executes, successors)
import Modelling.PetriNet.Reach.Type (
  ArrowDensityConstraints(..),
  Capacity (Unbounded),
  Net (..),
  Place (..),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  State (State),
  Transition (..),
  TransitionBehaviorConstraints,
  TransitionsList (TransitionsList),
  bimapNet,
  countFusableInputNodes,
  countFusableOutputNodes,
  example,
  noArrowDensityConstraints,
  noTransitionBehaviorConstraints,
  )

import Control.Applicative              (Alternative, (<|>))
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  Rated,
  english,
  german,
  translate,
  yesNo,
  )
import Data.Ratio                       ((%))

import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Data.Bifunctor                   (bimap)
import Data.Either.Combinators          (whenRight)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (guard)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (whenJust)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.Random       (RandT)
import Data.Maybe                       (fromMaybe)
import System.Random.Internal           (StdGen)
import Data.GraphViz                    (GraphvizCommand (..))
#if !MIN_VERSION_base(4,18,0)
import Data.Typeable                    (Typeable)
#endif
import GHC.Generics                     (Generic)

verifyDeadlock
  :: (OutputCapable m, Show a, Show t, Ord t, Ord a)
  => DeadlockInstance a t
  -> LangM m
verifyDeadlock = validate Default . petriNet

deadlockTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputCapable m,
    Show s,
    Show t
    )
  => Bool
  -> FilePath
  -> DeadlockInstance s t
  -> LangM m
deadlockTask showInputHelp path inst = do
  lift (drawToFile (not $ showPlaceNames inst) path (drawUsing inst) (petriNet inst))
  $>>= \img ->
    reportReachFor
    showInputHelp
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (minLength inst)
    (withMinLengthHint inst)
    Nothing

deadlockInitial :: DeadlockInstance s Transition -> TransitionsList
deadlockInitial = TransitionsList . reverse . S.toList . transitions . petriNet

deadlockSyntax
  :: OutputCapable m
  => DeadlockInstance Place Transition
  -> [Transition]
  -> LangM m
deadlockSyntax inst ts =
  do transitionsValid (petriNet inst) ts
     isNoLonger (noLongerThan inst) ts
     rejectSpaceballsPattern (rejectSpaceballsLength inst) ts
     pure ()

deadlockEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> DeadlockInstance Place Transition
  -> [Transition]
  -> Rated m
deadlockEvaluation path deadlock ts =
  executes path (drawUsing deadlockInstance) n (map ShowTransition ts)
  $>>= \eitherOutcome ->
    whenRight eitherOutcome (\outcome ->
      yesNo (null $ successors n outcome)
      $ translate $ do
          english "All transitions disabled in reached marking?"
          german "Alle Transitionen deaktiviert in erreichter Markierung?"
      )
  $>> assertReachPoints
    aSolution
    (const $ null . successors n)
    minLength
    deadlockInstance
    ts
    eitherOutcome
  where
    deadlockInstance = toShowDeadlockInstance deadlock
    n = petriNet deadlockInstance
    aSolution = provideSolutionsFeedback (maxDisplayedSolutions deadlock) (shortestSolutions deadlock)

data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showPlaceNames    :: Bool,
  maxDisplayedSolutions :: Int,
  -- | Solutions to the deadlock task.
  -- 'Left' contains (some) shortest solutions when no filtering is applied.
  -- 'Right' contains all solutions when filtering is applied.
  -- Note: 'Left' may not contain all shortest solutions, only up to 'maxDisplayedSolutions'.
  shortestSolutions :: Either (NonEmpty [t]) (NonEmpty [t]),
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Bool,
  -- | Minimum length of Spaceballs PIN pattern to reject during syntax checking.
  -- If set to @Just n@, sequences starting with @n@ or more consecutive transitions
  -- (e.g., @[t1, t2, t3, t4]@) will be rejected.
  rejectSpaceballsLength :: Maybe Int
  } deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

bimapDeadlockInstance
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> DeadlockInstance s t
  -> DeadlockInstance a b
bimapDeadlockInstance f g DeadlockInstance {..} = DeadlockInstance {
    drawUsing         = drawUsing,
    minLength         = minLength,
    noLongerThan      = noLongerThan,
    petriNet          = bimapNet f g petriNet,
    showPlaceNames    = showPlaceNames,
    maxDisplayedSolutions = maxDisplayedSolutions,
    shortestSolutions = bimap (fmap (map g)) (fmap (map g)) shortestSolutions,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint,
    rejectSpaceballsLength = rejectSpaceballsLength
    }

toShowDeadlockInstance
  :: DeadlockInstance Place Transition
  -> DeadlockInstance ShowPlace ShowTransition
toShowDeadlockInstance = bimapDeadlockInstance ShowPlace ShowTransition

data DeadlockConfig = DeadlockConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  -- | Draw commands in order of preference
  drawPreferenceOrder :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  transitionBehaviorConstraints :: TransitionBehaviorConstraints,
  arrowDensityConstraints :: ArrowDensityConstraints,
  maxPrintedSolutions :: Int,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool,
  showPlaceNamesInNet :: Bool,
  -- | Require exactly this many transitions with exactly one input place,
  -- which is exclusively consumed from by that transition.
  -- If @Nothing@, no constraint on fusable input nodes.
  requireFusableInputNodes :: Maybe Int,
  -- | Require exactly this many transitions with exactly one output place,
  -- which is exclusively produced to by that transition.
  -- If @Nothing@, no constraint on fusable output nodes.
  requireFusableOutputNodes :: Maybe Int,
  filterConfig        :: FilterConfig
  }
  deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

defaultDeadlockConfig :: DeadlockConfig
defaultDeadlockConfig =
  DeadlockConfig {
  numPlaces = 6,
  numTransitions = 6,
  Modelling.PetriNet.Reach.Deadlock.capacity = Unbounded,
  drawPreferenceOrder = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  transitionBehaviorConstraints = noTransitionBehaviorConstraints,
  arrowDensityConstraints = noArrowDensityConstraints,
  maxPrintedSolutions = 0,
  rejectLongerThan    = Just 8,
  showLengthHint      = False,
  showMinLengthHint   = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Nothing,
  requireFusableOutputNodes = Nothing,
  filterConfig        = defaultFilterConfig { solutionSetLimit = Nothing, forbiddenCycleLengths = [4], requireCycleLengthsAny = [], transitionCoverageRequirement = 1 % 2 }
  }

defaultDeadlockInstance :: DeadlockInstance Place Transition
defaultDeadlockInstance = DeadlockInstance {
  drawUsing         = Circo,
  minLength         = 6,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showPlaceNames    = False,
  maxDisplayedSolutions = 0,
  shortestSolutions = Left ([] :| []), -- TO DO: add a solution
  withLengthHint    = Just 9,
  withMinLengthHint = True,
  rejectSpaceballsLength = Nothing
  }

checkFusableNodeConfig
  :: Maybe Int  -- ^ requireFusableInputNodes
  -> Maybe Int  -- ^ requireFusableOutputNodes
  -> Int        -- ^ numTransitions
  -> Int        -- ^ numPlaces
  -> ArrowDensityConstraints
  -> Maybe String
checkFusableNodeConfig maybeInputNodes maybeOutputNodes numTrans _ ArrowDensityConstraints {..}
  | Just count <- maybeInputNodes
  , count < 0 || count > numTrans
  = Just "requireFusableInputNodes must be non-negative and cannot exceed numTransitions"
  | Just count <- maybeOutputNodes
  , count < 0 || count > numTrans
  = Just "requireFusableOutputNodes must be non-negative and cannot exceed numTransitions"
  | Just inputCount <- maybeInputNodes
  , Just outputCount <- maybeOutputNodes
  , inputCount + outputCount > numTrans
  = Just "requireFusableInputNodes + requireFusableOutputNodes cannot exceed numTransitions"
  | Just inputCount <- maybeInputNodes
  , let (minimumIncoming, _) = incomingArrowsPerTransition
  , minimumIncoming > 1
  , inputCount > 0
  = Just "requireFusableInputNodes > 0 conflicts with incomingArrowsPerTransition minimum > 1"
  | Just inputCount <- maybeInputNodes
  , let (_, maximumIncoming) = incomingArrowsPerTransition
  , maximumIncoming == Just 0
  , inputCount > 0
  = Just "requireFusableInputNodes > 0 conflicts with incomingArrowsPerTransition maximum = 0"
  | Just outputCount <- maybeOutputNodes
  , let (minimumOutgoing, _) = outgoingArrowsPerTransition
  , minimumOutgoing > 1
  , outputCount > 0
  = Just "requireFusableOutputNodes > 0 conflicts with outgoingArrowsPerTransition minimum > 1"
  | Just outputCount <- maybeOutputNodes
  , let (_, maximumOutgoing) = outgoingArrowsPerTransition
  , maximumOutgoing == Just 0
  , outputCount > 0
  = Just "requireFusableOutputNodes > 0 conflicts with outgoingArrowsPerTransition maximum = 0"
  | Just inputCount <- maybeInputNodes
  , let (minimumOutgoingPlace, _) = outgoingArrowsPerPlace
  , minimumOutgoingPlace > 1
  , inputCount > 0
  = Just "requireFusableInputNodes > 0 conflicts with outgoingArrowsPerPlace minimum > 1"
  | Just inputCount <- maybeInputNodes
  , let (_, maximumOutgoingPlace) = outgoingArrowsPerPlace
  , maximumOutgoingPlace == Just 0
  , inputCount > 0
  = Just "requireFusableInputNodes > 0 conflicts with outgoingArrowsPerPlace maximum = 0"
  | Just outputCount <- maybeOutputNodes
  , let (minimumIncomingPlace, _) = incomingArrowsPerPlace
  , minimumIncomingPlace > 1
  , outputCount > 0
  = Just "requireFusableOutputNodes > 0 conflicts with incomingArrowsPerPlace minimum > 1"
  | Just outputCount <- maybeOutputNodes
  , let (_, maximumIncomingPlace) = incomingArrowsPerPlace
  , maximumIncomingPlace == Just 0
  , outputCount > 0
  = Just "requireFusableOutputNodes > 0 conflicts with incomingArrowsPerPlace maximum = 0"
  | Just inputCount <- maybeInputNodes
  , let (_, maximumTotal) = totalArrowsFromPlacesToTransitions
  , inputCount > 0
  , Just maxTotal <- maximumTotal
  , maxTotal < inputCount
  = Just "requireFusableInputNodes exceeds totalArrowsFromPlacesToTransitions upper bound"
  | Just outputCount <- maybeOutputNodes
  , let (_, maximumTotal) = totalArrowsFromTransitionsToPlaces
  , outputCount > 0
  , Just maxTotal <- maximumTotal
  , maxTotal < outputCount
  = Just "requireFusableOutputNodes exceeds totalArrowsFromTransitionsToPlaces upper bound"
  | otherwise
  = Nothing

checkDeadlockConfig :: DeadlockConfig -> Maybe String
checkDeadlockConfig DeadlockConfig {..} =
  checkBasicPetriConfig
    numPlaces
    numTransitions
    capacity
    minTransitionLength
    maxTransitionLength
    transitionBehaviorConstraints
    arrowDensityConstraints
    drawPreferenceOrder
    rejectLongerThan
    showLengthHint
  <|>
  checkFilterConfigWith
    rejectLongerThan
    minTransitionLength
    numTransitions
    filterConfig
  <|>
  if maxPrintedSolutions < 0
    then Just "maxPrintedSolutions must be non-negative"
    else case solutionSetLimit filterConfig of
      Just maxSolutions | maxPrintedSolutions > maxSolutions ->
        Just "maxPrintedSolutions cannot be greater than solutionSetLimit"
      _ -> Nothing
  <|>
  checkFusableNodeConfig
    requireFusableInputNodes
    requireFusableOutputNodes
    numTransitions
    numPlaces
    arrowDensityConstraints

generateDeadlock
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => DeadlockConfig
  -> Int
  -> m (DeadlockInstance Place Transition)
generateDeadlock conf@DeadlockConfig {..} seed = do
  (petri, cmd, solutionsList) <- tries conf seed
  pure DeadlockInstance {
    drawUsing         = cmd,
    minLength         = minTransitionLength,
    noLongerThan      = rejectLongerThan,
    petriNet          = petri,
    showPlaceNames    = showPlaceNamesInNet,
    maxDisplayedSolutions = maxPrintedSolutions,
    shortestSolutions = solutionsList,
    withLengthHint    =
      if showLengthHint then Just maxTransitionLength else Nothing,
    withMinLengthHint = showMinLengthHint,
    rejectSpaceballsLength = spaceballsPrefixThreshold filterConfig
    }

tries
  :: forall m. (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => DeadlockConfig
  -> Int
  -> m (Net Place Transition, GraphvizCommand, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
tries conf seed = eval out
  where
    eval f = evalRandT f $ mkStdGen seed
    out
      :: RandT StdGen m (Net Place Transition, GraphvizCommand, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
    out =
      maybe out pure =<< runMaybeT (try conf)

try
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => DeadlockConfig
  -> MaybeT (RandT StdGen m) (Net Place Transition, GraphvizCommand, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
try conf = do
    let ps = [Place 1 .. Place (numPlaces conf)]
        ts = [Transition 1 .. Transition (numTransitions conf)]
    n <- MaybeT $ netLimitsFiltered
      (arrowDensityConstraints conf)
      (numPlaces conf)
      ps
      ts
      (Modelling.PetriNet.Reach.Deadlock.capacity conf)
      (transitionBehaviorConstraints conf)
      (fromMaybe 0 $ requireFusableInputNodes conf)
      (fromMaybe 0 $ requireFusableOutputNodes conf)
    -- Check fusable node constraints
    whenJust (requireFusableInputNodes conf) $ \expected ->
      guard $ countFusableInputNodes n == expected
    whenJust (requireFusableOutputNodes conf) $ \expected ->
      guard $ countFusableOutputNodes n == expected
    let deadlockLevels = map (filter (null . successors n . fst)) (levelsWithAlternatives n)
        (no, yeah) = span null
          $ take (maxTransitionLength conf + 1)
          deadlockLevels
    guard $ not $ null yeah
    let allShortestSolutions = map reverse . concatMap snd $ head yeah
    guard $ length no >= minTransitionLength conf
    (cmd, solutionsList) <- validateDrawabilityAndSolutionFiltering
      n (drawPreferenceOrder conf) allShortestSolutions
      (filterConfig conf) (numTransitions conf) (maxPrintedSolutions conf)
    pure (n, cmd, solutionsList)

exampleInstance :: Net Int Int
exampleInstance =
  Net {
  places = S.fromList [1, 2, 3, 4, 5],
  transitions = S.fromList [1, 2, 3, 4, 5],
  connections = [
      ([1], 1, [1, 2, 3]),
      ([2], 2, [3, 4]),
      ([3], 3, [4, 5]),
      ([4], 4, [5, 1]),
      ([5], 5, [1, 2]),
      ([1, 2, 3, 4, 5], 7, [])
      ],
    Modelling.PetriNet.Reach.Type.capacity = Unbounded,
    start = State $ M.fromList [(1, 1), (2, 0), (3, 0), (4, 0), (5, 0)]
  }
