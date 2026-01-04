{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList)

import Modelling.PetriNet.Reach.Type (
  Net (..),
  Capacity,
  State (State),
  Connection,
  TransitionBehaviorConstraints,
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

-- | Generate a net with limits and apply standard filters
-- This combines net generation with filtering for isolated nodes and transition behavior constraints
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => (Int, Maybe Int)                  -- ^ preconditionsRange
  -> (Int, Maybe Int)                  -- ^ postconditionsRange
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFiltered preconditionsRange postconditionsRange numPlaces ps ts capacityConstraint transitionBehaviorConstraints = do
  n <- netLimits vLow vHigh nLow nHigh ps ts capacityConstraint
  return $ do
    -- Filter out nets with isolated nodes
    guard $ not $ hasIsolatedNodes n
    -- Filter out nets that don't satisfy transition behavior constraints
    guard $ satisfiesTransitionBehaviorConstraints n transitionBehaviorConstraints
    return n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum preconditionsRange
    (nLow, nHigh) = fixMaximum postconditionsRange
