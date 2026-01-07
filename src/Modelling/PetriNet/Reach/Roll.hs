{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered) where

import qualified Data.Map                         as M (fromList, findWithDefault, fromListWith)
import qualified Data.Set                         as S (fromList, toList)

import Modelling.PetriNet.Reach.Type (
  Net (..),
  Capacity,
  State (State),
  Connection,
  TransitionBehaviorConstraints,
  ArrowDensityConstraints (..),
  hasIsolatedNodes,
  satisfiesTransitionBehaviorConstraints,
  inBounds,
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

-- | Check per-place arrow constraints efficiently by sharing computation
-- of concatenated pre/post lists
checkPerPlaceConstraints
  :: Ord s
  => Net s t
  -> (Int, Maybe Int)  -- ^ incomingArrowsPerPlace bounds
  -> (Int, Maybe Int)  -- ^ outgoingArrowsPerPlace bounds
  -> Bool
checkPerPlaceConstraints net incomingBounds outgoingBounds
  | incomingBounds == (0, Nothing) && outgoingBounds == (0, Nothing) = True
  | otherwise =
      checkIncoming incomingBounds && checkOutgoing outgoingBounds
  where
    -- Concatenate and group all post lists (transitions to places) - computed once
    allTransToPlaces = concatMap (\(_, _, post) -> post) (connections net)
    incomingCountMap = M.fromListWith (+) [(place, 1) | place <- allTransToPlaces]

    -- Concatenate and group all pre lists (places to transitions) - computed once
    allPlacesToTrans = concatMap (\(pre, _, _) -> pre) (connections net)
    outgoingCountMap = M.fromListWith (+) [(place, 1) | place <- allPlacesToTrans]

    checkIncoming (0, Nothing) = True
    checkIncoming bounds =
      all (\place ->
          let count = M.findWithDefault 0 place incomingCountMap
          in inBounds bounds count
        ) (S.toList $ places net)

    checkOutgoing (0, Nothing) = True
    checkOutgoing bounds =
      all (\place ->
          let count = M.findWithDefault 0 place outgoingCountMap
          in inBounds bounds count
        ) (S.toList $ places net)

-- | Check total arrow constraints efficiently by sharing computation
checkTotalArrowConstraints
  :: Net s t
  -> (Int, Maybe Int)  -- ^ totalArrowsFromPlacesToTransitions bounds
  -> (Int, Maybe Int)  -- ^ totalArrowsFromTransitionsToPlaces bounds
  -> Bool
checkTotalArrowConstraints net placesToTransBounds transToPlacesBounds
  | placesToTransBounds == (0, Nothing) && transToPlacesBounds == (0, Nothing) = True
  | otherwise =
      checkPlacesToTrans placesToTransBounds && checkTransToPlaces transToPlacesBounds
  where
    -- Compute totals once
    totalPlacesToTrans = sum [length pre | (pre, _, _) <- connections net]
    totalTransToPlaces = sum [length post | (_, _, post) <- connections net]

    checkPlacesToTrans (0, Nothing) = True
    checkPlacesToTrans bounds = inBounds bounds totalPlacesToTrans

    checkTransToPlaces (0, Nothing) = True
    checkTransToPlaces bounds = inBounds bounds totalTransToPlaces

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
  arrowConstraints
  numPlaces
  ps
  ts
  capacityConstraint
  transitionBehaviorConstraints = do
  n <- netLimits vLow vHigh nLow nHigh ps ts capacityConstraint
  return $ do
    -- Filter out nets with isolated nodes
    guard $ not $ hasIsolatedNodes n
    -- Filter out nets that don't satisfy transition behavior constraints
    guard $ satisfiesTransitionBehaviorConstraints n transitionBehaviorConstraints
    -- Filter out nets that don't satisfy arrow density constraints (optimized)
    guard $ checkPerPlaceConstraints n
      (incomingArrowsPerPlace arrowConstraints)
      (outgoingArrowsPerPlace arrowConstraints)
    guard $ checkTotalArrowConstraints n
      (totalArrowsFromPlacesToTransitions arrowConstraints)
      (totalArrowsFromTransitionsToPlaces arrowConstraints)
    return n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum (incomingArrowsPerTransition arrowConstraints)
    (nLow, nHigh) = fixMaximum (outgoingArrowsPerTransition arrowConstraints)
