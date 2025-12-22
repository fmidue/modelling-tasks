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
  areSolutionsTrivial,
  defaultFilterConfig,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkFilterConfigWith,
  checkMaxPrintedSolutions,
  )
import Modelling.PetriNet.Reach.Reach   (
  assertReachPoints,
  isNoLonger,
  reportReachFor,
  transitionsValid,
  formatSolutionsFeedback,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (executes, levelsWithAlternatives, successors)
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
import Data.Bifunctor                   (Bifunctor (second), bimap)
import Data.Either.Combinators          (whenRight)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (guard, msum, replicateM)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (findM)
import Control.Monad.Random             (MonadRandom, evalRandT, mkStdGen)
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import System.Random.Shuffle            (shuffleM)
import Data.GraphViz                    (GraphvizCommand (..))
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
    aSolution = formatSolutionsFeedback (maxDisplayedSolutions deadlock) (solutions deadlock)

{-|
Get all possible shortest solutions for deadlock detection in a given Petri net

Note: This function does not terminate
if no deadlock is reachable and the net is not bounded.
-}
deadlockAllSolutions :: Ord s => Net s t -> [[t]]
deadlockAllSolutions net =
  map reverse . concatMap snd
    $ head $ dropWhile null
    $ map (filter (null . successors net . fst)) $ levelsWithAlternatives net

data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showPlaceNames    :: Bool,
  maxDisplayedSolutions :: Int,
  solutions         :: Either [t] [[t]],
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
    maxDisplayedSolutions = maxDisplayedSolutions,
    solutions         = bimap (map g) (map (map g)) solutions,
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
  maxPrintedSolutions :: Int,
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
  maxPrintedSolutions = 0,
  rejectLongerThan    = Just 8,
  showLengthHint      = False,
  showMinLengthHint   = True,
  showPlaceNamesInNet = False,
  filterConfig        = defaultFilterConfig { maxNumberOfSolutions = Nothing }
  }

defaultDeadlockInstance :: DeadlockInstance Place Transition
defaultDeadlockInstance = DeadlockInstance {
  drawUsing         = Circo,
  minLength         = 6,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showPlaceNames    = False,
  maxDisplayedSolutions = 0,
  solutions         = Left [], -- TO DO: add a solution
  withLengthHint    = Just 9,
  withMinLengthHint = True
  }

checkDeadlockConfig :: DeadlockConfig -> Maybe String
checkDeadlockConfig DeadlockConfig {..} =
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
  <|>
  checkFilterConfigWith
    rejectLongerThan
    minTransitionLength
    maxTransitionLength
    filterConfig
  <|>
  checkMaxPrintedSolutions maxPrintedSolutions filterConfig

generateDeadlock
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => DeadlockConfig
  -> Int
  -> m (DeadlockInstance Place Transition)
generateDeadlock conf@DeadlockConfig {..} seed = do
  (petri, cmd, solutionsList) <- tries 1000 filterConfig conf seed
  pure DeadlockInstance {
    drawUsing         = cmd,
    minLength         = minTransitionLength,
    noLongerThan      = rejectLongerThan,
    petriNet          = petri,
    showPlaceNames    = showPlaceNamesInNet,
    maxDisplayedSolutions = maxPrintedSolutions,
    solutions         = solutionsList,
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
  -> m (Net Place Transition, GraphvizCommand, Either [Transition] [[Transition]])
tries n filterConfig conf seed = eval out
  where
    eval f = evalRandT f $ mkStdGen seed
    out = do
      xs <- replicateM n $ try conf
      maybe out pure =<< runMaybeT (msum $ map checkCandidate $ concat xs)
    checkCandidate (l, pn, singleSolution, allShortestSolutions) = do
      guard $ l >= minTransitionLength conf
      let availableTransitions = transitions pn
      guard (not $ areSolutionsTrivial filterConfig availableTransitions allShortestSolutions)
      cmd <- MaybeT $ findM (Monad.lift . isPetriDrawable pn) (drawCommands conf)
      solutionsList <-
        if filterConfig == noFiltering
          then pure $ Left singleSolution
          else Right <$> Monad.lift (shuffleM allShortestSolutions)
      pure (pn, cmd, solutionsList)

try :: MonadRandom m => DeadlockConfig -> m [(Int, Net Place Transition, [Transition], [[Transition]])]
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
    let levelsWithAlts = levelsWithAlternatives n
        deadlockLevels = map (filter (null . successors n . fst)) levelsWithAlts
        (no, yeah) = span null
          $ take (maxTransitionLength conf + 1)
          deadlockLevels
    guard $ not $ null yeah
    let firstDeadlock = head $ head yeah
        solutionSequence = reverse $ head $ snd firstDeadlock
        allShortestSolutions = map reverse . concatMap snd $ head yeah
    return (length no, n, solutionSequence, allShortestSolutions)
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
