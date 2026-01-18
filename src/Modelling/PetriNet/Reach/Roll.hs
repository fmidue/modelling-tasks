{-# LANGUAGE RecordWildCards #-}
{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered) where

import qualified Data.Bimap                       as BM (
  empty,
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

-- | Generate net with preexisting connections and forbid sets
netLimitsWithPregenerated
  :: (MonadRandom m, Ord s, Ord t)
  => Int  -- ^ vLow
  -> Int  -- ^ vHigh
  -> Int  -- ^ nLow
  -> Int  -- ^ nHigh
  -> [s]  -- ^ places
  -> [t]  -- ^ transitions
  -> Capacity s
  -> [Connection s t]  -- ^ Pre-generated connections
  -> BM.Bimap t s  -- ^ Bimap from fusable consuming-transitions to their input places
  -> BM.Bimap t s  -- ^ Bimap from fusable producing-transitions to their output places
  -> m (Net s t)
netLimitsWithPregenerated
  vLow vHigh nLow nHigh ps ts cap
  pregeneratedConnections
  transitionConsumingBimap transitionProducingBimap = do
  s <- state ps
  -- Generate connections for ALL transitions, respecting forbid sets
  newConnections <- forM ts $ \t -> do
    (vor, nach) <- generateValidConnection t
    return (vor, t, nach)
  -- Merge pregenerated and new connections
  -- Optimization: skip merge if no pregenerated connections
  let finalConnections = if null pregeneratedConnections
                         then newConnections
                         else let pregeneratedMap = M.fromList [(t, (pre, post)) | (pre, t, post) <- pregeneratedConnections]
                              in map (\(vor, t, nach) ->
                                   case M.lookup t pregeneratedMap of
                                     Just (preVor, preNach) -> (preVor ++ vor, t, preNach ++ nach)
                                     Nothing -> (vor, t, nach)
                                 ) newConnections
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = finalConnections,
    capacity    = cap,
    start       = s
    }
  where
    generateValidConnection t = do
      vor <- if BM.member t transitionConsumingBimap
             then return []
             else takeRandom vLow vHigh ps
      nach <- if BM.member t transitionProducingBimap
              then return []
              else takeRandom nLow nHigh ps
      -- Check both input and output place usage
      if isValidInputPlaceUsage t vor nach && isValidOutputPlaceUsage t vor nach
        then return (vor, nach)
        else generateValidConnection t  -- Retry if invalid

    isValidInputPlaceUsage t vor nach =
      -- Skip checks if input bimap is empty
      BM.null transitionConsumingBimap ||
      (  -- For each place in vor: if it's a forbidden input place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionConsumingBimap) || (vor == [place] && nach == [place])) vor
         -- If t has a pregenerated input place, prevent that place from appearing in nach
         && maybe True (`notElem` nach) (BM.lookup t transitionConsumingBimap)
      )

    isValidOutputPlaceUsage t vor nach =
      -- Skip checks if output bimap is empty
      BM.null transitionProducingBimap ||
      (  -- For each place in nach: if it's a forbidden output place, only allow if vor == nach == [that place]
         all (\place -> not (BM.memberR place transitionProducingBimap) || (vor == [place] && nach == [place])) nach
         -- If t has a pregenerated output place, prevent that place from appearing in vor
         && maybe True (`notElem` vor) (BM.lookup t transitionProducingBimap)
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

-- | Result of generating fusable connections
data FusableConnectionsResult s t = FusableConnectionsResult
  { pregeneratedConnections :: [Connection s t]
  , transitionConsumingBimap :: BM.Bimap t s
  , transitionProducingBimap :: BM.Bimap t s
  }

-- | Create connections for fusable consuming-transitions (place -> transition)
createConsumingConnections :: [s] -> [t] -> [Connection s t]
createConsumingConnections =
  zipWith (\place trans -> ([place], trans, []))

-- | Create connections for fusable producing-transitions (transition -> place)
createProducingConnections :: [t] -> [s] -> [Connection s t]
createProducingConnections =
  zipWith (\trans place -> ([], trans, [place]))

-- | Create a bimap from transitions to places
createTransitionPlaceBimap :: (Ord t, Ord s) => [t] -> [s] -> BM.Bimap t s
createTransitionPlaceBimap transitions places = BM.fromList $ zip transitions places

-- | Selected transitions and places for fusable connections
data FusableSelections s t = FusableSelections
  { consumingTransitions :: [t]
  , consumingPlaces :: [s]
  , producingTransitions :: [t]
  , producingPlaces :: [s]
  }

-- | Randomly select transitions and places for fusable connections
selectFusableTransitionsAndPlaces
  :: MonadRandom m
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of consuming fusable transitions
  -> Int  -- ^ Number of producing fusable transitions
  -> m (FusableSelections s t)
selectFusableTransitionsAndPlaces allPlaces allTransitions numConsuming numProducing = do
  shuffledTransitions <- shuffleM allTransitions
  shuffledPlaces <- shuffleM allPlaces
  let (consumingTrans, remainingTransitions) = splitAt numConsuming shuffledTransitions
      producingTrans = take numProducing remainingTransitions
      (consumingPl, remainingPlaces) = splitAt numConsuming shuffledPlaces
      producingPl = take numProducing remainingPlaces
  return FusableSelections
    { consumingTransitions = consumingTrans
    , consumingPlaces = consumingPl
    , producingTransitions = producingTrans
    , producingPlaces = producingPl
    }

-- | Generate pre-determined fusable node connections
generateFusableConnections
  :: (MonadRandom m, Ord t, Ord s)
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of fusable consuming-transitions to create
  -> Int  -- ^ Number of fusable producing-transitions to create
  -> m (FusableConnectionsResult s t)
generateFusableConnections allPlaces allTransitions numConsumingFusable numProducingFusable = do
  -- Randomly select transitions and places for fusable nodes
  FusableSelections{..} <- selectFusableTransitionsAndPlaces allPlaces allTransitions numConsumingFusable numProducingFusable
  -- Create connections and bimaps
  let inputConnections = createConsumingConnections consumingPlaces consumingTransitions
      outputConnections = createProducingConnections producingTransitions producingPlaces
      consumingBimap = createTransitionPlaceBimap consumingTransitions consumingPlaces
      producingBimap = createTransitionPlaceBimap producingTransitions producingPlaces
  -- Return connections and transition-place bimaps
  return FusableConnectionsResult
    { pregeneratedConnections = inputConnections ++ outputConnections
    , transitionConsumingBimap = consumingBimap
    , transitionProducingBimap = producingBimap
    }

-- | Helper to generate fusable connections or return empty values if not needed
generateFusableConnectionsIfNeeded
  :: (MonadRandom m, Ord s, Ord t)
  => [s]  -- ^ places
  -> [t]  -- ^ transitions
  -> Int  -- ^ required minimum fusable transitions consuming
  -> Int  -- ^ required minimum fusable transitions producing
  -> m (FusableConnectionsResult s t)
generateFusableConnectionsIfNeeded places transitions requiredConsuming requiredProducing
  | requiredConsuming == 0 && requiredProducing == 0 =
      return FusableConnectionsResult
        { pregeneratedConnections = []
        , transitionConsumingBimap = BM.empty
        , transitionProducingBimap = BM.empty
        }
  | otherwise = generateFusableConnections places transitions requiredConsuming requiredProducing

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints
--
-- The parameters @requiredFusableTransitionsConsuming@ and @requiredFusableTransitionsProducing@
-- specify the minimum numbers of transitions with the respective properties.
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> Int                               -- ^ required minimum fusable transitions consuming
  -> Int                               -- ^ required minimum fusable transitions producing
  -> m (Maybe (Net s t))
netLimitsFiltered
  ArrowDensityConstraints{..}
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints
  requiredFusableTransitionsConsuming
  requiredFusableTransitionsProducing = do
  -- Pre-generate fusable node connections
  FusableConnectionsResult{..} <-
    generateFusableConnectionsIfNeeded ps ts requiredFusableTransitionsConsuming requiredFusableTransitionsProducing
  -- Generate net with forbid sets
  n <- netLimitsWithPregenerated vLow vHigh nLow nHigh ps ts capacityConstraint
         pregeneratedConnections transitionConsumingBimap transitionProducingBimap
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
