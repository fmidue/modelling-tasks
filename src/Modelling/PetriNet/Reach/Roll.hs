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
  union,
  )
import qualified Data.Set                         as S (empty, fromList, member, Set)

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

netConns
  :: (MonadRandom m, Ord s, Ord t)
  => ([s] -> [t] -> m [Connection s t])
  -> [s]
  -> [t]
  -> Capacity s
  -> m (Net s t)
netConns conns ps ts cap = do
  s <- state ps
  cs <- conns ps ts
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = cs,
    capacity    = cap,
    start       = s
    }

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

netLimits
  :: (MonadRandom m, Ord s, Ord t)
  => Int
  -> Int
  -> Int
  -> Int
  -> [s]
  -> [t]
  -> Capacity s
  -> m (Net s t)
netLimits vLow vHigh nLow nHigh = netConns $ connLimits vLow vHigh nLow nHigh

connLimits
  :: MonadRandom m
  => Int
  -> Int
  -> Int
  -> Int
  -> [s]
  -> [t]
  -> m [Connection s t]
connLimits vLow vHigh nLow nHigh ps ts = forM ts $ \t -> do
  vor <- takeRandom vLow vHigh ps
  nach <- takeRandom nLow nHigh ps
  return (vor, t, nach)

takeRandom :: MonadRandom m => Int -> Int -> [a] -> m [a]
takeRandom low high xs  = take
  <$> getRandomR (low, high)
  <*> shuffleM xs

-- | Tracks transitions and places that are reserved for fusable nodes
data FusableReservations s t = FusableReservations {
  -- | Transitions that should not receive additional incoming arrows
  noMoreIncoming :: S.Set t,
  -- | Transitions that should not receive additional outgoing arrows
  noMoreOutgoing :: S.Set t,
  -- | Places that should not receive additional outgoing arrows (to transitions)
  noMoreFromPlace :: S.Set s,
  -- | Places that should not receive additional incoming arrows (from transitions)
  noMoreToPlace :: S.Set s
  }

emptyReservations :: FusableReservations s t
emptyReservations = FusableReservations {
  noMoreIncoming = S.empty,
  noMoreOutgoing = S.empty,
  noMoreFromPlace = S.empty,
  noMoreToPlace = S.empty
  }

-- | Check if a connection would violate fusable node reservations
isAllowedConnection
  :: (Ord s, Ord t)
  => FusableReservations s t
  -> [Connection s t]  -- ^ Existing connections (to check for loops)
  -> s                 -- ^ Source place
  -> t                 -- ^ Transition
  -> s                 -- ^ Destination place
  -> Bool
isAllowedConnection FusableReservations{..} existingConns sourcePlace trans destPlace =
  -- Check if transition is allowed to have this incoming arrow
  not (S.member trans noMoreIncoming || S.member sourcePlace noMoreFromPlace)
  -- Check if transition is allowed to have this outgoing arrow
  && not (S.member trans noMoreOutgoing || S.member destPlace noMoreToPlace)
  -- Allow loops: if there's already a connection from trans to sourcePlace, allow sourcePlace -> trans
  || hasLoop
  where
    hasLoop = any (\(_, t, post) -> t == trans && sourcePlace `elem` post) existingConns
            || any (\(pre, t, _) -> t == trans && destPlace `elem` pre) existingConns

-- | Generate pre-determined fusable node connections
generateFusableConnections
  :: (MonadRandom m, Ord s, Ord t)
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of input-fusable transitions to create
  -> Int  -- ^ Number of output-fusable transitions to create
  -> m ([Connection s t], FusableReservations s t)
generateFusableConnections allPlaces allTransitions numInputFusable numOutputFusable = do
  -- Randomly select transitions and places for fusable nodes
  shuffledTransitions <- shuffleM allTransitions
  shuffledPlaces <- shuffleM allPlaces
  let inputFusableTransitions = take numInputFusable shuffledTransitions
      outputFusableTransitions = take numOutputFusable (drop numInputFusable shuffledTransitions)
      inputFusablePlaces = take numInputFusable shuffledPlaces
      outputFusablePlaces = take numOutputFusable (drop numInputFusable shuffledPlaces)
  -- Create connections for input-fusable transitions
  let inputConnections = zipWith (\place trans -> ([place], trans, [])) inputFusablePlaces inputFusableTransitions
  -- Create connections for output-fusable transitions
  let outputConnections = zipWith (\trans place -> ([], trans, [place])) outputFusableTransitions outputFusablePlaces
  -- Build reservations
  let reservations = FusableReservations {
        noMoreIncoming = S.fromList inputFusableTransitions,
        noMoreOutgoing = S.fromList outputFusableTransitions,
        noMoreFromPlace = S.fromList inputFusablePlaces,
        noMoreToPlace = S.fromList outputFusablePlaces
        }
  return (inputConnections ++ outputConnections, reservations)

-- | Generate net with pre-existing connections and reservations
netLimitsWithReservations
  :: (MonadRandom m, Ord s, Ord t)
  => Int  -- ^ vLow
  -> Int  -- ^ vHigh
  -> Int  -- ^ nLow
  -> Int  -- ^ nHigh
  -> [s]  -- ^ places
  -> [t]  -- ^ transitions
  -> Capacity s
  -> [Connection s t]  -- ^ Pre-generated connections
  -> FusableReservations s t
  -> m (Net s t)
netLimitsWithReservations vLow vHigh nLow nHigh ps ts cap pregenConns reservations = do
  s <- state ps
  -- Get transitions that already have connections
  let transitionsWithConnections = [t | (_, t, _) <- pregenConns]
      transitionsNeedingConnections = filter (`notElem` transitionsWithConnections) ts
  -- Generate connections for remaining transitions
  newConns <- forM transitionsNeedingConnections $ \t -> do
    let allConns = pregenConns  -- We'll work with what we have so far
    vor <- takeRandomFiltered vLow vHigh ps $ \place ->
      not (S.member t (noMoreIncoming reservations) || S.member place (noMoreFromPlace reservations))
      || any (\(_, trans, post) -> trans == t && place `elem` post) allConns
    nach <- takeRandomFiltered nLow nHigh ps $ \place ->
      not (S.member t (noMoreOutgoing reservations) || S.member place (noMoreToPlace reservations))
      || any (\(pre, trans, _) -> trans == t && place `elem` pre) allConns
    return (vor, t, nach)
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = pregenConns ++ newConns,
    capacity    = cap,
    start       = s
    }

-- | Take random elements from a list that satisfy a predicate
takeRandomFiltered :: MonadRandom m => Int -> Int -> [a] -> (a -> Bool) -> m [a]
takeRandomFiltered low high xs predicate = do
  count <- getRandomR (low, high)
  shuffled <- shuffleM xs
  return $ take count $ filter predicate shuffled

-- | Helper to check if a value satisfies the given bounds
inBounds :: (Int, Maybe Int) -> Int -> Bool
inBounds (low, maybeHigh) value =
  value >= low && maybe True (value <=) maybeHigh

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> Int                               -- ^ required fusable input nodes (0 for Reach tasks)
  -> Int                               -- ^ required fusable output nodes (0 for Reach tasks)
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
  (pregenConnections, reservations) <- generateFusableConnections
    ps ts requiredFusableInputNodes requiredFusableOutputNodes
  -- Generate remaining connections with restrictions
  n <- netLimitsWithReservations vLow vHigh nLow nHigh ps ts capacityConstraint
         pregenConnections reservations
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
