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
  lookup,
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

-- | Merge pregenerated connections with newly generated connections
mergeConnections
  :: Ord t
  => [Connection s t]  -- ^ Pregenerated connections
  -> [Connection s t]  -- ^ New connections
  -> [Connection s t]  -- ^ Merged connections
mergeConnections pregeneratedConnections newConnections =
  if null pregeneratedConnections
  then newConnections
  else let pregeneratedMap = M.fromList [(t, (pre, post)) | (pre, t, post) <- pregeneratedConnections]
       in map (\(vor, t, nach) ->
            case M.lookup t pregeneratedMap of
              Just (preVor, preNach) -> (preVor ++ vor, t, preNach ++ nach)
              Nothing -> (vor, t, nach)
          ) newConnections

-- | Generate net with preexisting connections and forbid sets
netLimitsWithPregenerated
  :: (MonadRandom m, Ord s, Ord t)
  => [s]  -- ^ places
  -> [t]  -- ^ transitions
  -> Capacity s
  -> (t -> m ([s], [s]))  -- ^ Function to generate valid connection for a transition
  -> ([Connection s t] -> [Connection s t])  -- ^ Function to merge connections
  -> m (Net s t)
netLimitsWithPregenerated
  ps ts cap
  genValidConn
  mergeConns = do
  s <- state ps
  -- Generate connections for ALL transitions, respecting forbid sets
  newConnections <- forM ts $ \t -> do
    (vor, nach) <- genValidConn t
    return (vor, t, nach)
  -- Merge pregenerated and new connections
  let finalConnections = mergeConns newConnections
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = finalConnections,
    capacity    = cap,
    start       = s
    }

-- | Generate a valid connection for a transition with retry logic
generateValidConnection
  :: (MonadRandom m, Ord s, Ord t)
  => BM.Bimap t s  -- ^ Bimap from fusable consuming-transitions to their input places
  -> BM.Bimap t s  -- ^ Bimap from fusable producing-transitions to their output places
  -> Int           -- ^ vLow
  -> Int           -- ^ vHigh
  -> Int           -- ^ nLow
  -> Int           -- ^ nHigh
  -> [s]           -- ^ places
  -> t             -- ^ Transition
  -> m ([s], [s])  -- ^ (vor, nach)
generateValidConnection transitionConsumingBimap transitionProducingBimap vLow vHigh nLow nHigh ps t = do
      vor <- if BM.member t transitionConsumingBimap
             then return []
             else takeRandom vLow vHigh ps
      nach <- if BM.member t transitionProducingBimap
              then return []
              else takeRandom nLow nHigh ps
      -- Check both input and output place usage
      if isValidInputPlaceUsage t vor nach && isValidOutputPlaceUsage t vor nach
        then return (vor, nach)
        else generateValidConnection transitionConsumingBimap transitionProducingBimap vLow vHigh nLow nHigh ps t  -- Retry if invalid
  where
    -- | Check if input place usage is valid for a transition
    isValidInputPlaceUsage theTransition vor nach =
      -- Skip checks if input bimap is empty
      BM.null transitionConsumingBimap ||
      (  -- For each place in vor: if it's a forbidden input place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionConsumingBimap) || (vor == [place] && nach == [place])) vor
         -- If t has a pregenerated input place, prevent that place from appearing in nach
         && maybe True (`notElem` nach) (BM.lookup theTransition transitionConsumingBimap)
      )

    -- | Check if output place usage is valid for a transition
    isValidOutputPlaceUsage theTransition vor nach =
      -- Skip checks if output bimap is empty
      BM.null transitionProducingBimap ||
      (  -- For each place in nach: if it's a forbidden output place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionProducingBimap) || (vor == [place] && nach == [place])) nach
         -- If t has a pregenerated output place, prevent that place from appearing in vor
         && maybe True (`notElem` vor) (BM.lookup theTransition transitionProducingBimap)
      )

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
  -> m ([Connection s t], BM.Bimap t s, BM.Bimap t s)
generateFusableConnections allPlaces allTransitions numConsumingFusable numProducingFusable = do
  -- Randomly select transitions and places for fusable nodes
  shuffledTransitions <- shuffleM allTransitions
  shuffledPlaces <- shuffleM allPlaces
  let (inputFusableTransitions, remainingTransitions) = splitAt numConsumingFusable shuffledTransitions
      outputFusableTransitions = take numProducingFusable remainingTransitions
      (placesForInputFusableTransitions, remainingPlaces) = splitAt numConsumingFusable shuffledPlaces
      placesForOutputFusableTransitions = take numProducingFusable remainingPlaces
  -- Create connections for fusable consuming-transitions (s -> t)
  let inputConnections = zipWith (\place trans -> ([place], trans, []))
                                  placesForInputFusableTransitions inputFusableTransitions
  -- Create connections for fusable producing-transitions (t -> s)
  let outputConnections = zipWith (\trans place -> ([], trans, [place]))
                                   outputFusableTransitions placesForOutputFusableTransitions
  -- Create bimaps from transitions to their pregenerated places
  let transitionConsumingBimap = BM.fromList $ zip inputFusableTransitions placesForInputFusableTransitions
      transitionProducingBimap = BM.fromList $ zip outputFusableTransitions placesForOutputFusableTransitions
  -- Return connections and transition-place bimaps
  return ( inputConnections ++ outputConnections
         , transitionConsumingBimap  -- bimap from fusable consuming-transitions to their places
         , transitionProducingBimap  -- bimap from fusable producing-transitions to their places
         )

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints,
-- with pregenerated fusable connections
netLimitsFilteredWith
  :: (MonadRandom m, Ord s, Ord t)
  => [Connection s t]                  -- ^ Pre-generated connections
  -> BM.Bimap t s                      -- ^ Bimap from fusable consuming-transitions to their input places
  -> BM.Bimap t s                      -- ^ Bimap from fusable producing-transitions to their output places
  -> ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFilteredWith
  pregeneratedConnections
  transitionConsumingBimap
  transitionProducingBimap
  arrowDensityConstraints
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints =
  netLimitsFilteredCommon
    arrowDensityConstraints
    numPlaces
    ps
    ts
    capacityConstraint
    transitionBehaviorConstraints
    (generateValidConnection transitionConsumingBimap transitionProducingBimap)
    (mergeConnections pregeneratedConnections)

-- | Common implementation for netLimitsFiltered variants
netLimitsFilteredCommon
  :: (MonadRandom m, Ord s, Ord t)
  => ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> (Int -> Int -> Int -> Int -> [s] -> t -> m ([s], [s]))  -- ^ Function to generate valid connection
  -> ([Connection s t] -> [Connection s t])  -- ^ Function to merge connections
  -> m (Maybe (Net s t))
netLimitsFilteredCommon
  arrowDensityConstraints
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints
  genValidConn
  mergeConns = do
  n <- netLimitsWithPregenerated ps ts capacityConstraint
         (genValidConn vLow vHigh nLow nHigh ps)
         mergeConns
  return $ applyNetFiltering arrowDensityConstraints transitionBehaviorConstraints n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum (incomingArrowsPerTransition arrowDensityConstraints)
    (nLow, nHigh) = fixMaximum (outgoingArrowsPerTransition arrowDensityConstraints)

-- | Apply filtering checks to a net based on arrow density constraints and transition behavior
applyNetFiltering
  :: (Ord s, Ord t)
  => ArrowDensityConstraints
  -> TransitionBehaviorConstraints
  -> Net s t
  -> Maybe (Net s t)
applyNetFiltering ArrowDensityConstraints{..} transitionBehaviorConstraints n = do
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
netLimitsFiltered
  arrowDensityConstraints
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints =
  netLimitsFilteredCommon
    arrowDensityConstraints
    numPlaces
    ps
    ts
    capacityConstraint
    transitionBehaviorConstraints
    (\vLow vHigh nLow nHigh thePlaces _ -> takeRandom vLow vHigh thePlaces >>= \vor -> takeRandom nLow nHigh thePlaces >>= \nach -> return (vor, nach))
    id
