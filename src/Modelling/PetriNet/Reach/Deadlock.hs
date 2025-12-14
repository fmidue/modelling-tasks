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
{-# LANGUAGE TupleSections #-}

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

  -- * Solutions
  deadlockSolution,
  deadlockAllSolutions,

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

import qualified Control.Monad.Trans              as Monad (lift)
import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.PetriNet.Reach.Draw    (drawToFile, isPetriDrawable)
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  defaultFilterConfig,
  isTrivialSequence,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  )
import Modelling.PetriNet.Reach.Reach   (
  assertReachPoints,
  isNoLonger,
  levelsWithAlternatives,
  reportReachFor,
  transitionsValid,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (deadlocks, deadlocks', executes, successors)
import Modelling.PetriNet.Reach.Type (
  Capacity (Unbounded),
  Net (..),
  Place (..),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  State (State),
  Transition (..),
  TransitionsList (TransitionsList),
  bimapNet,
  example,
  hasIsolatedNodes,
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
import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Data.Bifunctor                   (Bifunctor (second))
import Data.Either.Combinators          (whenRight)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (guard, msum, replicateM)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (findM)
import Control.Monad.Random             (MonadRandom, evalRandT, mkStdGen)
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (find)
import Data.Maybe                       (fromMaybe)
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
    aSolution
      | showSolution deadlockInstance
      = Just $ show $ TransitionsList $ deadlockSolution deadlock
      | otherwise
      = Nothing

deadlockSolution :: Ord s => DeadlockInstance s t -> [t]
deadlockSolution = reverse . snd . head . concat . deadlocks' . petriNet

{-|
Get all possible shortest solutions for deadlock detection in a given Petri net

Note: This function does not terminate
if no deadlock is reachable and the net is not bounded.
-}
deadlockAllSolutions :: Ord s => Net s t -> [[t]]
deadlockAllSolutions network =
  reverse . maybe [] snd $ find (null . successors network . fst)
    $ concat $ levelsWithAlternatives network

data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showPlaceNames    :: Bool,
  showSolution      :: Bool,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Bool
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
    showSolution      = showSolution,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint
    }

toShowDeadlockInstance
  :: DeadlockInstance Place Transition
  -> DeadlockInstance ShowPlace ShowTransition
toShowDeadlockInstance = bimapDeadlockInstance ShowPlace ShowTransition

data DeadlockConfig = DeadlockConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  postconditionsRange :: (Int, Maybe Int),
  preconditionsRange  :: (Int, Maybe Int),
  printSolution       :: Bool,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool,
  showPlaceNamesInNet :: Bool,
  filterConfig        :: FilterConfig
  }
  deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

defaultDeadlockConfig :: DeadlockConfig
defaultDeadlockConfig =
  DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Deadlock.capacity = Unbounded,
  drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  postconditionsRange = (0, Nothing),
  preconditionsRange  = (0, Nothing),
  printSolution       = False,
  rejectLongerThan    = Just 8,
  showLengthHint      = True,
  showMinLengthHint   = True,
  showPlaceNamesInNet = False,
  filterConfig        = defaultFilterConfig
  }

defaultDeadlockInstance :: DeadlockInstance Place Transition
defaultDeadlockInstance = DeadlockInstance {
  drawUsing         = Circo,
  minLength         = 6,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showPlaceNames    = False,
  showSolution      = False,
  withLengthHint    = Just 9,
  withMinLengthHint = True
  }

checkDeadlockConfig :: DeadlockConfig -> Maybe String
checkDeadlockConfig config@DeadlockConfig {..} =
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
  <|> checkFilterConfig config

checkFilterConfig :: DeadlockConfig -> Maybe String
checkFilterConfig DeadlockConfig {..}
  | rejectLongerThan /= Just minTransitionLength
  , filterConfig /= noFiltering
  = Just $ "If transition length is not enforced to one value, filterConfig must be set to "
    ++ show noFiltering
  | Just repeats <- minRepetitiveLength filterConfig
  , repeats < 2
  = Just "minRepetitiveLength has to be set to at least 2 if it is enabled"
  | Just repeats <- minRepetitiveLength filterConfig
  , repeats > maxTransitionLength `div` 2
  = Just "minRepetitiveLength must not be higher than half of maxTransitionLength if it is enabled"
  | Just cycleLength <- maxCycleLength filterConfig
  , cycleLength < 1
  = Just "setting maxCycleLength to less than 1 does not make sense"
  | Just cycleLength <- maxCycleLength filterConfig
  , cycleLength > maxTransitionLength `div` 2
  = Just "maxCycleLength must not be higher than half of maxTransitionLength if it is enabled"
  | Just spaceballsLength <- minSpaceballsLength filterConfig
  , spaceballsLength < 2
  = Just "setting minSpaceballsLength to less than 2 does not make sense"
  | Just spaceballsLength <- minSpaceballsLength filterConfig
  , spaceballsLength > maxTransitionLength
  = Just "minSpaceballsLength must not be higher than maxTransitionLength if it is enabled"
  | otherwise
  = Nothing

generateDeadlock
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => DeadlockConfig
  -> Int
  -> m (DeadlockInstance Place Transition)
generateDeadlock conf@DeadlockConfig {..} seed = do
  (petri, cmd) <- tries 1000 filterConfig conf seed
  pure DeadlockInstance {
    drawUsing         = cmd,
    minLength         = minTransitionLength,
    noLongerThan      = rejectLongerThan,
    petriNet          = petri,
    showPlaceNames    = showPlaceNamesInNet,
    showSolution      = printSolution,
    withLengthHint    =
      if showLengthHint then Just maxTransitionLength else Nothing,
    withMinLengthHint = showMinLengthHint
    }

tries
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => Int
  -> FilterConfig
  -> DeadlockConfig
  -> Int
  -> m (Net Place Transition, GraphvizCommand)
tries n filterConfig conf seed = eval out
  where
    eval f = evalRandT f $ mkStdGen seed
    out = do
      xs <- replicateM n $ try conf
      let candidates = concat xs
      maybeResult <- runMaybeT $ msum $ map checkCandidate candidates
      maybe out pure maybeResult
    checkCandidate (pathLength, network) = do
      guard $ pathLength >= minTransitionLength conf
      let allSolutions = deadlockAllSolutions network
      guard (not $ any (isTrivialSequence filterConfig) allSolutions)
      MaybeT $ fmap (network,) <$> findM (Monad.lift . isPetriDrawable network) (drawCommands conf)

try :: MonadRandom m => DeadlockConfig -> m [(Int, Net Place Transition)]
try conf = do
  let ps = [Place 1 .. Place (numPlaces conf)]
      ts = [Transition 1 .. Transition (numTransitions conf)]
  n <- netLimits vLow vHigh nLow nHigh
      ps
      ts
      (Modelling.PetriNet.Reach.Deadlock.capacity conf)
  return $ do
    -- Filter out nets with isolated nodes
    guard $ not $ hasIsolatedNodes n
    let (no,yeah) = span (null . snd)
          $ take (maxTransitionLength conf + 1)
          $ zip [0 :: Int ..]
          $ deadlocks n
    guard $ not $ null yeah
    return (length no, n)
  where
    fixMaximum = second (min (numPlaces conf) . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum $ preconditionsRange conf
    (nLow, nHigh) = fixMaximum $ postconditionsRange conf

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
