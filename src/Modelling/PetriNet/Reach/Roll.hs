{-# LANGUAGE RecordWildCards #-}
{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered, netLimitsFilteredWith, generateFusableConnections) where

import qualified Data.Bimap                       as BM (
  fromList,
  lookup,
  member,
  memberR,
  null,
  Bimap,
  )
import qualified Data.Map                         as M (
  fromList,
  fromListWith,
  fromSet,
  elems,
  union,
  )
import qualified Data.Set                         as S (fromList)

import Modelling.PetriNet.Reach.Type (
  Net (..),
  Capacity,
  State (State),
  Connection,
  TransitionBehaviorConstraints,
  ArrowDensityConstraints (..),
  hasIsolatedNodes,
  satisfiesTransitionBehaviorConstraints,
  )

import Control.Monad                    (forM, guard)
import Control.Monad.Random.Class       (MonadRandom (getRandomR))
import Data.Maybe                       (fromMaybe)
import System.Random.Shuffle            (shuffleM)

-- | Generate a valid connection for a transition with retry logic
generateValidConnection
  :: (MonadRandom m, Ord s, Ord t)
  => BM.Bimap t s  -- ^ Bimap from fusable consuming-transitions to their input places
  -> BM.Bimap t s  -- ^ Bimap from fusable consuming-transitions to their output places
  -> t             -- ^ Transition
  -> m [s]         -- ^ Action to get input places
  -> m [s]         -- ^ Action to get output places
  -> m ([s], [s])  -- ^ (vor, nach)
generateValidConnection transitionConsumingBimap transitionProducingBimap =
  \t inputPlacesAction outputPlacesAction ->
  let
    go = do
      vor <- if BM.member t transitionConsumingBimap
             then return []
             else inputPlacesAction
      nach <- if BM.member t transitionProducingBimap
              then return []
              else outputPlacesAction
      -- Check both input and output place usage
      if isValidInputPlaceUsage t vor nach && isValidOutputPlaceUsage t vor nach
        then return (vor, nach)
        else go  -- Retry if invalid
  in go
  where
    -- | Check if input place usage is valid for a transition
    isValidInputPlaceUsage =
      if BM.null transitionConsumingBimap
      then \_ _ _ -> True
      else \t vor nach ->
         -- For each place in vor: if it's a forbidden input place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionConsumingBimap) || (vor == [place] && nach == [place])) vor
         -- If t has a pregenerated input place, prevent that place from appearing in nach
         && maybe True (`notElem` nach) (BM.lookup t transitionConsumingBimap)

    -- | Check if output place usage is valid for a transition
    isValidOutputPlaceUsage =
      if BM.null transitionProducingBimap
      then \_ _ _ -> True
      else \t vor nach ->
         -- For each place in nach: if it's a forbidden output place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionProducingBimap) || (vor == [place] && nach == [place])) nach
         -- If t has a pregenerated output place, prevent that place from appearing in vor
         && maybe True (`notElem` vor) (BM.lookup t transitionProducingBimap)

state :: (MonadRandom m, Ord s) => [s] -> m (State s)
state ps = do
  qs <- selection ps
  return $ State $ M.fromList $ do
    p <- ps
    return (p, if p `elem` qs then 1 else 0)

{- | pick a non-empty subset,
 size s with probability 2^-s
-}
selection :: MonadRandom m => [a] -> m [a]
selection [] = return []
selection xs = do
  i <- getRandomR (0, length xs - 1)
  let (pre,x:post) = splitAt i xs
  f <- getRandomR (False, True)
  xs' <- if f then selection $ pre ++ post else return []
  return $ x : xs'

takeRandom :: MonadRandom m => Int -> Int -> [a] -> m [a]
takeRandom low high xs  = take
  <$> getRandomR (low, high)
  <*> shuffleM xs

-- | Helper to check if a value satisfies the given bounds
inBounds :: (Int, Maybe Int) -> Int -> Bool
inBounds (low, maybeHigh) value =
  value >= low && maybe True (value <=) maybeHigh

-- | Generate pre-determined fusable node connections
generateFusableConnections
  :: (MonadRandom m, Ord t, Ord s)
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of fusable consuming-transitions to create
  -> Int  -- ^ Number of fusable producing-transitions to create
  -> m (BM.Bimap t s, BM.Bimap t s)
generateFusableConnections allPlaces allTransitions numConsumingFusable numProducingFusable = do
  -- Randomly select transitions and places for fusable nodes
  shuffledTransitions <- shuffleM allTransitions
  shuffledPlaces <- shuffleM allPlaces
  let (inputFusableTransitions, remainingTransitions) = splitAt numConsumingFusable shuffledTransitions
      outputFusableTransitions = take numProducingFusable remainingTransitions
      (placesForInputFusableTransitions, remainingPlaces) = splitAt numConsumingFusable shuffledPlaces
      placesForOutputFusableTransitions = take numProducingFusable remainingPlaces
  -- Create bimaps from transitions to their pregenerated places
  let transitionConsumingBimap = BM.fromList $ zip inputFusableTransitions placesForInputFusableTransitions
      transitionProducingBimap = BM.fromList $ zip outputFusableTransitions placesForOutputFusableTransitions
  -- Return transition-place bimaps
  return ( transitionConsumingBimap  -- bimap from fusable consuming-transitions to their places
         , transitionProducingBimap  -- bimap from fusable producing-transitions to their places
         )

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints,
-- with pregenerated fusable connections
netLimitsFilteredWith
  :: (MonadRandom m, Ord s, Ord t)
  => BM.Bimap t s                      -- ^ Bimap from fusable consuming-transitions to their input places
  -> BM.Bimap t s                      -- ^ Bimap from fusable producing-transitions to their output places
  -> ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFilteredWith
  transitionConsumingBimap
  transitionProducingBimap =
  netLimitsFilteredCommon
    $ \t inputPlacesAction outputPlacesAction -> do
        (vor, nach) <- generateValidConnection transitionConsumingBimap transitionProducingBimap t inputPlacesAction outputPlacesAction
        case BM.lookup t transitionConsumingBimap of
          Just preVor
            -> return (preVor : vor, t, nach)
          _
            -> case BM.lookup t transitionProducingBimap of
                 Just preNach
                   -> return (vor, t, preNach : nach)
                 _
                   -> return (vor, t, nach)
          -- impossible for both lookups to return Just

-- | Common implementation for netLimitsFiltered variants
netLimitsFilteredCommon
  :: (MonadRandom m, Ord s, Ord t)
  => (t -> m [s] -> m [s] -> m (Connection s t))  -- ^ Function to generate a connection for a transition
  -> ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFilteredCommon
  generateConnection
  ArrowDensityConstraints{..}
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints = do
  s <- state ps
  -- Generate connections for ALL transitions, respecting forbid sets
  theConnections <- forM ts $ \t ->
    generateConnection t (takeRandom vLow vHigh ps) (takeRandom nLow nHigh ps)
  let n = Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = theConnections,
    capacity    = capacityConstraint,
    start       = s
    }
  return $ do
    -- Filter out nets with isolated nodes
    guard $ not $ hasIsolatedNodes n
    -- Filter out nets that don't satisfy transition behavior constraints
    guard $ satisfiesTransitionBehaviorConstraints n transitionBehaviorConstraints
    -- Filter out nets that don't satisfy arrow density constraints beyond incomingArrowsPerTransition and outgoingArrowsPerTransition
    let allTransToPlaces = concatMap (\(_, _, post) -> post) (connections n)
    let allPlacesToTrans = concatMap (\(pre, _, _) -> pre) (connections n)
    let initialMap = M.fromSet (const 0) (places n)
    guard $ case incomingArrowsPerPlace of
      (0, Nothing) -> True
      _ -> let countMap = M.fromListWith (+) [(place, 1) | place <- allTransToPlaces]
                          `M.union` initialMap
           in all (inBounds incomingArrowsPerPlace) $ M.elems countMap
    guard $ case outgoingArrowsPerPlace of
      (0, Nothing) -> True
      _ -> let countMap = M.fromListWith (+) [(place, 1) | place <- allPlacesToTrans]
                          `M.union` initialMap
           in all (inBounds outgoingArrowsPerPlace) $ M.elems countMap
    guard $ case totalArrowsFromPlacesToTransitions of
      (0, Nothing) -> True
      _ -> inBounds totalArrowsFromPlacesToTransitions (length allPlacesToTrans)
    guard $ case totalArrowsFromTransitionsToPlaces of
      (0, Nothing) -> True
      _ -> inBounds totalArrowsFromTransitionsToPlaces (length allTransToPlaces)
    return n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum incomingArrowsPerTransition
    (nLow, nHigh) = fixMaximum outgoingArrowsPerTransition

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFiltered =
  netLimitsFilteredCommon
    $ \t inputPlacesAction outputPlacesAction -> do
        (vor, nach) <- liftA2 (,) inputPlacesAction outputPlacesAction
        return (vor, t, nach)
