{-# LANGUAGE RecordWildCards #-}
{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered) where

import qualified Data.Map                         as M (
  fromList,
  fromListWith,
  fromSet,
  elems,
  lookup,
  member,
  union,
  Map,
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
  -> [s]  -- ^ Places used in pregenerated input connections (forbid in vor unless loop)
  -> [s]  -- ^ Places used in pregenerated output connections (forbid in nach unless loop)
  -> M.Map t s  -- ^ Map from transitions to their pregenerated input place
  -> M.Map t s  -- ^ Map from transitions to their pregenerated output place
  -> m (Net s t)
netLimitsWithPregenerated
  vLow vHigh nLow nHigh ps ts cap
  pregeneratedConnections
  forbiddenInputPlaces forbiddenOutputPlaces
  transitionInputMap transitionOutputMap = do
  s <- state ps
  -- Generate connections for ALL transitions, respecting forbid sets
  newConnections <- forM ts $ \t -> do
    (vor, nach) <- generateValidConnection t
    return (vor, t, nach)
  -- Merge pregenerated and new connections
  let pregeneratedMap = M.fromList [(t, (pre, post)) | (pre, t, post) <- pregeneratedConnections]
      mergedConnections = map (\(vor, t, nach) ->
        case M.lookup t pregeneratedMap of
          Just (preVor, preNach) -> (preVor ++ vor, t, preNach ++ nach)
          Nothing -> (vor, t, nach)
        ) newConnections
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = mergedConnections,
    capacity    = cap,
    start       = s
    }
  where
    generateValidConnection t = do
      vor <- if M.member t transitionInputMap
             then return []
             else takeRandom vLow vHigh ps
      nach <- if M.member t transitionOutputMap
              then return []
              else takeRandom nLow nHigh ps
      -- Check if the connection violates place forbid rules
      if isValidPlaceUsage t vor nach
        then return (vor, nach)
        else generateValidConnection t  -- Retry if invalid

    isValidPlaceUsage t vor nach =
      -- For each place in vor: if it's a forbidden input place, only allow if vor == nach == [that place]
      all (\place -> place `notElem` forbiddenInputPlaces || (vor == [place] && nach == [place])) vor
      -- For each place in nach: if it's a forbidden output place, only allow if vor == nach == [that place]
      && all (\place -> place `notElem` forbiddenOutputPlaces || (vor == [place] && nach == [place])) nach
      -- If t has a pregenerated input place, prevent that place from appearing in nach
      && maybe True (`notElem` nach) (M.lookup t transitionInputMap)
      -- If t has a pregenerated output place, prevent that place from appearing in vor
      && maybe True (`notElem` vor) (M.lookup t transitionOutputMap)

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
  :: (MonadRandom m, Ord t)
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of input-fusable transitions to create
  -> Int  -- ^ Number of output-fusable transitions to create
  -> m ([Connection s t], [s], [s], M.Map t s, M.Map t s)
generateFusableConnections allPlaces allTransitions numInputFusable numOutputFusable = do
  -- Randomly select transitions and places for fusable nodes
  shuffledTransitions <- shuffleM allTransitions
  shuffledPlaces <- shuffleM allPlaces
  let inputFusableTransitions = take numInputFusable shuffledTransitions
      outputFusableTransitions = take numOutputFusable (drop numInputFusable shuffledTransitions)
      inputFusablePlaces = take numInputFusable shuffledPlaces
      outputFusablePlaces = take numOutputFusable (drop numInputFusable shuffledPlaces)
  -- Create connections for input-fusable transitions (s -> t)
  let inputConnections = zipWith (\place trans -> ([place], trans, []))
                                  inputFusablePlaces inputFusableTransitions
  -- Create connections for output-fusable transitions (t -> s)
  let outputConnections = zipWith (\trans place -> ([], trans, [place]))
                                   outputFusableTransitions outputFusablePlaces
  -- Create maps from transitions to their pregenerated place (single place, not list)
  let transitionInputMap = M.fromList $ zip inputFusableTransitions inputFusablePlaces
      transitionOutputMap = M.fromList $ zip outputFusableTransitions outputFusablePlaces
  -- Return connections, place forbid sets, and transition-place maps
  return ( inputConnections ++ outputConnections
         , inputFusablePlaces        -- forbid these places in vor (unless loop)
         , outputFusablePlaces       -- forbid these places in nach (unless loop)
         , transitionInputMap        -- map from transitions to their pregenerated input place
         , transitionOutputMap       -- map from transitions to their pregenerated output place
         )

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> Int                               -- ^ required fusable input nodes
  -> Int                               -- ^ required fusable output nodes
  -> m (Maybe (Net s t))
netLimitsFiltered
  ArrowDensityConstraints{..}
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints
  requiredFusableInputNodes
  requiredFusableOutputNodes = do
  -- Pre-generate fusable node connections
  (pregeneratedConnections, forbiddenInputPlaces, forbiddenOutputPlaces, transitionInputMap, transitionOutputMap) <-
    generateFusableConnections ps ts requiredFusableInputNodes requiredFusableOutputNodes
  -- Generate net with forbid sets
  n <- netLimitsWithPregenerated vLow vHigh nLow nHigh ps ts capacityConstraint
         pregeneratedConnections forbiddenInputPlaces forbiddenOutputPlaces transitionInputMap transitionOutputMap
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
