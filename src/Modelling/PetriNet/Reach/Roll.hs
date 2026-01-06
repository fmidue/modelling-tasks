{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Roll.hs
-}
module Modelling.PetriNet.Reach.Roll (netLimitsFiltered) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

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
import Data.Maybe                       (fromMaybe, isJust, isNothing)
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

-- | Generate a net with limits and filtering for isolated nodes and transition behavior constraints
netLimitsFiltered
  :: (MonadRandom m, Ord s, Ord t)
  => (Int, Maybe Int)                  -- ^ incomingArrowsPerTransition
  -> (Int, Maybe Int)                  -- ^ outgoingArrowsPerTransition
  -> (Int, Maybe Int)                  -- ^ incomingArrowsPerPlace
  -> (Int, Maybe Int)                  -- ^ outgoingArrowsPerPlace
  -> (Int, Maybe Int)                  -- ^ totalArrowsFromPlacesToTransitions
  -> (Int, Maybe Int)                  -- ^ totalArrowsFromTransitionsToPlaces
  -> Int                               -- ^ numPlaces
  -> [s]                               -- ^ places
  -> [t]                               -- ^ transitions
  -> Capacity s                        -- ^ capacityConstraint
  -> TransitionBehaviorConstraints     -- ^ transition behavior constraints
  -> m (Maybe (Net s t))
netLimitsFiltered
  incomingArrowsPerTransition
  outgoingArrowsPerTransition
  incomingArrowsPerPlace
  outgoingArrowsPerPlace
  totalArrowsFromPlacesToTransitions
  totalArrowsFromTransitionsToPlaces
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
    -- Filter out nets that don't satisfy per-place arrow constraints
    guard $ satisfiesPerPlaceConstraints n incomingArrowsPerPlace outgoingArrowsPerPlace
    -- Filter out nets that don't satisfy total arrow constraints
    guard $ satisfiesTotalArrowConstraints n totalArrowsFromPlacesToTransitions totalArrowsFromTransitionsToPlaces
    return n
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum (low, high) = (low, fromMaybe numPlaces high)
    (vLow, vHigh) = fixMaximum incomingArrowsPerTransition
    (nLow, nHigh) = fixMaximum outgoingArrowsPerTransition

-- | Check if a net satisfies per-place arrow constraints
satisfiesPerPlaceConstraints
  :: Ord s
  => Net s t
  -> (Int, Maybe Int)  -- ^ incomingArrowsPerPlace
  -> (Int, Maybe Int)  -- ^ outgoingArrowsPerPlace
  -> Bool
satisfiesPerPlaceConstraints net (incomingLow, incomingHigh) (outgoingLow, outgoingHigh)
  -- Special case: if both lower bounds are 0 and both upper bounds are Nothing, no checking needed
  | incomingLow == 0 && isNothing incomingHigh && outgoingLow == 0 && isNothing outgoingHigh = True
  | otherwise = all checkPlace (S.toList $ places net)
  where
    checkPlace place =
      let incoming = if incomingLow > 0 || isJust incomingHigh
                     then countIncomingArrows place (connections net)
                     else 0  -- Skip counting if not needed
          outgoing = if outgoingLow > 0 || isJust outgoingHigh
                     then countOutgoingArrows place (connections net)
                     else 0  -- Skip counting if not needed
          incomingOk = (incomingLow == 0 || incoming >= incomingLow) &&
                      maybe True (incoming <=) incomingHigh
          outgoingOk = (outgoingLow == 0 || outgoing >= outgoingLow) &&
                      maybe True (outgoing <=) outgoingHigh
      in incomingOk && outgoingOk

    -- Count arrows coming into a place (from transitions to place)
    countIncomingArrows place conns =
      sum [count place post | (_, _, post) <- conns]
    -- Count arrows going out of a place (from place to transitions)
    countOutgoingArrows place conns =
      sum [count place pre | (pre, _, _) <- conns]
    count place list = length $ filter (== place) list

-- | Check if a net satisfies total arrow constraints
satisfiesTotalArrowConstraints
  :: Net s t
  -> (Int, Maybe Int)  -- ^ totalArrowsFromPlacesToTransitions
  -> (Int, Maybe Int)  -- ^ totalArrowsFromTransitionsToPlaces
  -> Bool
satisfiesTotalArrowConstraints net (placesToTransLow, placesToTransHigh) (transToPlacesLow, transToPlacesHigh)
  -- Special case: if both lower bounds are 0 and both upper bounds are Nothing, no checking needed
  | placesToTransLow == 0 && isNothing placesToTransHigh && transToPlacesLow == 0 && isNothing transToPlacesHigh = True
  | otherwise =
      let placesToTrans = if placesToTransLow > 0 || isJust placesToTransHigh
                          then sum [length pre | (pre, _, _) <- connections net]
                          else 0  -- Skip counting if not needed
          transToPlaces = if transToPlacesLow > 0 || isJust transToPlacesHigh
                          then sum [length post | (_, _, post) <- connections net]
                          else 0  -- Skip counting if not needed
          placesToTransOk = (placesToTransLow == 0 || placesToTrans >= placesToTransLow) &&
                           maybe True (placesToTrans <=) placesToTransHigh
          transToPlacesOk = (transToPlacesLow == 0 || transToPlaces >= transToPlacesLow) &&
                           maybe True (transToPlaces <=) transToPlacesHigh
      in placesToTransOk && transToPlacesOk
