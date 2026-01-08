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
  :: MonadRandom m
  => [s]  -- ^ All places
  -> [t]  -- ^ All transitions
  -> Int  -- ^ Number of input-fusable transitions to create
  -> Int  -- ^ Number of output-fusable transitions to create
  -> m ([Connection s t], [t], [t])
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
  -- Return connections and forbid sets
  return ( inputConnections ++ outputConnections
         , inputFusableTransitions   -- forbid incoming to these
         , outputFusableTransitions  -- forbid outgoing from these
         )

-- | Generate net with pre-existing connections and forbid sets
netLimitsWithPregen
  :: (MonadRandom m, Ord s, Ord t)
  => Int  -- ^ vLow
  -> Int  -- ^ vHigh
  -> Int  -- ^ nLow
  -> Int  -- ^ nHigh
  -> [s]  -- ^ places
  -> [t]  -- ^ transitions
  -> Capacity s
  -> [Connection s t]  -- ^ Pre-generated connections
  -> [t]  -- ^ Transitions that should not receive incoming connections
  -> [t]  -- ^ Transitions that should not have outgoing connections
  -> m (Net s t)
netLimitsWithPregen vLow vHigh nLow nHigh ps ts cap pregenConns forbidIncoming forbidOutgoing = do
  s <- state ps
  -- Get transitions that already have connections
  let transitionsWithConnections = [t | (_, t, _) <- pregenConns]
      transitionsNeedingConnections = filter (`notElem` transitionsWithConnections) ts
  -- Generate connections for remaining transitions using forbid sets
  newConns <- forM transitionsNeedingConnections $ \t -> do
    vor <- if t `elem` forbidIncoming
           then return []
           else takeRandom vLow vHigh ps
    nach <- if t `elem` forbidOutgoing
            then return []
            else takeRandom nLow nHigh ps
    return (vor, t, nach)
  return $ Net {
    places      = S.fromList ps,
    transitions = S.fromList ts,
    connections = pregenConns ++ newConns,
    capacity    = cap,
    start       = s
    }

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
  (pregenConnections, forbidIncoming, forbidOutgoing) <-
    generateFusableConnections ps ts requiredFusableInputNodes requiredFusableOutputNodes
  -- Generate net with forbid sets
  n <- netLimitsWithPregen vLow vHigh nLow nHigh ps ts capacityConstraint
         pregenConnections forbidIncoming forbidOutgoing
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
