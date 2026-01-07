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

-- | Check if a net satisfies incoming arrows per place constraint
satisfiesIncomingArrowsPerPlace
  :: Ord s
  => Net s t
  -> (Int, Maybe Int)  -- ^ incomingArrowsPerPlace
  -> [s]              -- ^ concatenated post lists (shared computation)
  -> Bool
satisfiesIncomingArrowsPerPlace _ (0, Nothing) _ = True
satisfiesIncomingArrowsPerPlace net bounds allTransToPlaces =
  all checkPlace (S.toList $ places net)
  where
    incomingCountMap = M.fromListWith (+) [(place, 1) | place <- allTransToPlaces]
    checkPlace place =
      let count = M.findWithDefault 0 place incomingCountMap
      in inBounds bounds count

-- | Check if a net satisfies outgoing arrows per place constraint
satisfiesOutgoingArrowsPerPlace
  :: Ord s
  => Net s t
  -> (Int, Maybe Int)  -- ^ outgoingArrowsPerPlace
  -> [s]              -- ^ concatenated pre lists (shared computation)
  -> Bool
satisfiesOutgoingArrowsPerPlace _ (0, Nothing) _ = True
satisfiesOutgoingArrowsPerPlace net bounds allPlacesToTrans =
  all checkPlace (S.toList $ places net)
  where
    outgoingCountMap = M.fromListWith (+) [(place, 1) | place <- allPlacesToTrans]
    checkPlace place =
      let count = M.findWithDefault 0 place outgoingCountMap
      in inBounds bounds count

-- | Check if a net satisfies total arrows from places to transitions constraint
satisfiesTotalPlacesToTransitions
  :: Net s t
  -> (Int, Maybe Int)  -- ^ totalArrowsFromPlacesToTransitions
  -> [s]              -- ^ concatenated pre lists (shared computation)
  -> Bool
satisfiesTotalPlacesToTransitions _ (0, Nothing) _ = True
satisfiesTotalPlacesToTransitions _ bounds allPlacesToTrans =
  inBounds bounds (length allPlacesToTrans)

-- | Check if a net satisfies total arrows from transitions to places constraint
satisfiesTotalTransitionsToPlaces
  :: Net s t
  -> (Int, Maybe Int)  -- ^ totalArrowsFromTransitionsToPlaces
  -> [s]              -- ^ concatenated post lists (shared computation)
  -> Bool
satisfiesTotalTransitionsToPlaces _ (0, Nothing) _ = True
satisfiesTotalTransitionsToPlaces _ bounds allTransToPlaces =
  inBounds bounds (length allTransToPlaces)

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
    -- Filter out nets that don't satisfy arrow density constraints
    -- Compute concatenated lists once and share across related checks
    let allTransToPlaces = concatMap (\(_, _, post) -> post) (connections n)
    let allPlacesToTrans = concatMap (\(pre, _, _) -> pre) (connections n)
    guard $ satisfiesIncomingArrowsPerPlace n
      (incomingArrowsPerPlace arrowConstraints) allTransToPlaces
    guard $ satisfiesOutgoingArrowsPerPlace n
      (outgoingArrowsPerPlace arrowConstraints) allPlacesToTrans
    guard $ satisfiesTotalPlacesToTransitions n
      (totalArrowsFromPlacesToTransitions arrowConstraints) allPlacesToTrans
    guard $ satisfiesTotalTransitionsToPlaces n
      (totalArrowsFromTransitionsToPlaces arrowConstraints) allTransToPlaces
    return n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum (incomingArrowsPerTransition arrowConstraints)
    (nLow, nHigh) = fixMaximum (outgoingArrowsPerTransition arrowConstraints)
