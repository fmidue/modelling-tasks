{-# LANGUAGE RecordWildCards #-}
{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered, simpleConnectionGenerator) where

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

-- | Generate a net with limits and filtering for isolated nodes and transition behaviour constraints,
-- potentially with pregenerated fusable connections (of which the makeUpdateConnection argument takes care)
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => (m [s] -> m [s] -> t -> m (Connection s t))  -- ^ Function to make/update a connection for a transition
  -> ArrowDensityConstraints           -- ^ arrow density constraints
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behaviour constraints
  -> m (Maybe (Net s t))
netLimitsFiltered
  makeUpdateConnection
  ArrowDensityConstraints{..}
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints = do
  s <- state ps
  -- Generate connections for ALL transitions, respecting forbid sets
  theConnections <- forM ts (makeUpdateConnection (takeRandom vLow vHigh ps) (takeRandom nLow nHigh ps))
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
    -- Filter out nets that don't satisfy transition behaviour constraints
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

-- | Simple connection generator without fusable connections
simpleConnectionGenerator
  :: Monad m
  => m [s] -> m [s] -> t -> m (Connection s t)
simpleConnectionGenerator inputPlacesAction outputPlacesAction t = do
  vor <- inputPlacesAction
  nach <- outputPlacesAction
  return (vor, t, nach)
