{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  validActionSequenceWithPetri,
  generateActionSequence,
  generateActionSequencesWithPetri,
  generateActionSequenceWithPetriAndRepetition,
  actionRepetitionDistance,
  netAndMap,
  computeActionSequenceLevels,
  getActionsLeadingToActivityFinals,
  isFinalPetriNode
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList, union, member, empty, toList, singleton, insert, notMember)
import qualified Data.Map as M (filter, map, keys, fromList, toList)

import Modelling.ActivityDiagram.Datatype (
  AdNode (..),
  UMLActivityDiagram (..),
  AdConnection (..),
  isActionNode,
  isActivityFinalNode
  )

import Modelling.ActivityDiagram.PetriNet (
  PetriKey(..),
  convertToPetriNet
  )

import Modelling.PetriNet.Types (
  PetriLike(..),
  Node(..),
  isPlaceNode, isTransitionNode
  )

import Modelling.PetriNet.Reach.Type (
  State(..),
  Capacity(..),
  Net(..)
  )

import Modelling.PetriNet.Reach.Step (successors)


import qualified Control.Monad as Monad (guard)
import Control.Monad.Random (MonadRandom, uniform)
import Data.List (find, union)
import Data.List.Extra (nubOrd)
import Data.Maybe (mapMaybe, isJust, fromJust)


fromPetriLike :: Ord a => PetriLike Node a -> Net a a
fromPetriLike petri =
  Net {
      places = S.fromList $ M.keys $ M.filter isPlaceNode $ allNodes petri,
      transitions = S.fromList $ M.keys $ M.filter isTransitionNode $ allNodes petri,
      connections = map (\(t,n) -> (M.keys $ flowIn n, t, M.keys $ flowOut n)) $ M.toList $ M.filter isTransitionNode $ allNodes petri,
      capacity = Unbounded,
      start = State {unState = M.map initial $ M.filter isPlaceNode $ allNodes petri}
  }

-- | Generate a valid action sequence reaching each of the final nodes
generateActionSequence :: UMLActivityDiagram -> [String]
generateActionSequence diag =
  head $ generateActionSequencesWithPetri diag (convertToPetriNet diag) Nothing

-- | Generate valid action sequences, using a pre-computed Petri net.
-- The returned list may be infinite or some of its tails even diverge,
-- if no length constraints are passed.
generateActionSequencesWithPetri
  :: UMLActivityDiagram
  -> PetriLike Node PetriKey
  -> Maybe (Int, Int)  -- Optional (minLength, maxLength) constraints
  -> [[String]]
generateActionSequencesWithPetri diag =
  let actionsLeadingToActivityFinals = getActionsLeadingToActivityFinals diag
  in generateSequencesWithLevels (levelsAS actionsLeadingToActivityFinals)

-- | Generate one valid action sequence with repetition, using a pre-computed Petri net.
-- This version allows cycle exploration to generate sequences with repeated actions.
-- Returns Nothing if no sequence with repetition can be found within the length constraints.
-- Uses randomness to select among sequences with equal maximum repetition distance.
generateActionSequenceWithPetriAndRepetition
  :: MonadRandom m
  => PetriLike Node PetriKey
  -> (Int, Int)  -- (minLength, maxLength) constraints
  -> Maybe (m [String])
generateActionSequenceWithPetriAndRepetition petri lengthBounds =
  let allActionSequences = generateSequencesWithLevels levelsWithCycles petri (Just lengthBounds)
      sequencesWithDistances = [(seq', d) | seq' <- allActionSequences, Just d <- [actionRepetitionDistance seq']]
  in if null sequencesWithDistances
     then Nothing
     else
       let maxDist = maximum $ map snd sequencesWithDistances
       in Just $ uniform [seq' | (seq', d) <- sequencesWithDistances, d == maxDist]

-- | Helper to generate sequences using a specific levels function
generateSequencesWithLevels
  :: (Net PetriKey PetriKey -> [[(State PetriKey, [PetriKey])]])
  -> PetriLike Node PetriKey
  -> Maybe (Int, Int)  -- Optional (minLength, maxLength) constraints
  -> [[String]]
generateSequencesWithLevels levelsFunction petriLike maybeLengthBounds =
  let petri = fromPetriLike petriLike
      -- Use all places in the network to create the zero state for consistency
      allPlaces = S.toList $ places petri
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
      relevantLevels = maybe id (\(minLength, maxLength) -> take (5 * maxLength) . drop minLength) maybeLengthBounds
                       $ levelsFunction petri
      convertAndFilterSequence transitionSequence =
        let actionSequence = [ actionName | NormalPetriNode {sourceNode = AdActionNode {name = actionName}} <- transitionSequence ]
            seqLength = length actionSequence
        in case maybeLengthBounds of
             Just (minLength, maxLength) | seqLength < minLength || seqLength > maxLength
               -> Nothing
             _ -> Just actionSequence
  in [ reverse a | level <- relevantLevels, (s, p) <- level, s == zeroState, Just a <- [convertAndFilterSequence p] ]

-- Modified version of levels' that handles Activity Final nodes
levelsAS :: Ord s => [Int] -> Net s PetriKey -> [[(State s, [PetriKey])]]
levelsAS actionsLeadingToActivityFinals n =
  let -- Create zero state using all places in the network for consistency
      allPlaces = S.toList $ places n
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
      -- Check if a transition corresponds to Activity Final
      isActivityFinalTransition t = case t of
        -- For normal petri nodes, check if the action leads to Activity Final
        NormalPetriNode {sourceNode = adNode} ->
          isActionNode adNode && Ad.label adNode `elem` actionsLeadingToActivityFinals
        -- For final petri nodes, check if it's an Activity Final
        FinalPetriNode {sourceNode = adNode} -> isActivityFinalNode adNode
        _ -> False
      f _ [] = []
      f done xs =
        let done' = S.fromList (map fst xs) `S.union` done
            next = M.toList $ M.fromList [ (finalState, t:p) |
                (x,p) <- xs,
                (t,y) <- successors n x,
                -- If this is an Activity Final transition, use consistent zero state
                let finalState = if isActivityFinalTransition t then zeroState else y,
                not $ S.member finalState done'
              ]
         in xs : f done' next
  in f S.empty [(start n, [])]

-- Get Action nodes that are immediately followed by Activity Final nodes
getActionsLeadingToActivityFinals :: UMLActivityDiagram -> [Int]
getActionsLeadingToActivityFinals (UMLActivityDiagram adNodes adConnections) =
  let activityFinalLabels = map Ad.label $ filter isActivityFinalNode adNodes
      -- Find action nodes that directly connect to Activity Final nodes
      directConnections = [(from conn, to conn) | conn <- adConnections,
                          to conn `elem` activityFinalLabels]
      actionNodeLabels = map Ad.label $ filter isActionNode adNodes
      actionsDirectlyToActivityFinals = [fromLabel | (fromLabel, _) <- directConnections,
                                        fromLabel `elem` actionNodeLabels]
  in actionsDirectlyToActivityFinals


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  uncurry (validActionSequenceWithPetri input diag) $ netAndMap $ convertToPetriNet diag

-- | Check if an action sequence is valid, using a pre-computed Petri net.
validActionSequenceWithPetri :: [String] -> UMLActivityDiagram -> Net PetriKey PetriKey -> [(String, PetriKey)] -> Bool
validActionSequenceWithPetri input diag net actionNameToPetriKey =
  let zeroState = State $ M.map (const 0) $ unState $ start net
      levels = computeActionSequenceLevels input net actionNameToPetriKey actionsLeadingToActivityFinals
      -- Get Action nodes that lead directly to Activity Final nodes
      actionsLeadingToActivityFinals = getActionsLeadingToActivityFinals diag
  in any (isJust . lookup zeroState) levels

netAndMap :: PetriLike Node PetriKey -> (Net PetriKey PetriKey, [(String, PetriKey)])
netAndMap petri =
  let -- Build map from action name to PetriKey by directly checking sourceNode
      actionNameToPetriKey =
        [ (actionName, k) | k@NormalPetriNode {sourceNode = AdActionNode {name = actionName}} <- M.keys $ allNodes petri ]
  in (fromPetriLike petri, actionNameToPetriKey)

-- | Common computation for action sequence validation.
computeActionSequenceLevels :: [String] -> Net PetriKey PetriKey -> [(String, PetriKey)] -> [Int] -> [[(State PetriKey, [PetriKey])]]
computeActionSequenceLevels input net actionNameToPetriKey actionsLeadingToActivityFinals =
  let -- Convert input action names to PetriKeys
      input' = mapMaybe (`lookup` actionNameToPetriKey) input
      -- Extract all action PetriKeys
      actions = map snd actionNameToPetriKey
      levels = levelsCheckAS input' actions net actionsLeadingToActivityFinals
  in levels

-- | Check if a PetriKey represents a final node transition
isFinalPetriNode :: PetriKey -> Bool
isFinalPetriNode (FinalPetriNode {}) = True
isFinalPetriNode _ = False

levelsCheckAS :: [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey -> [Int] -> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions n actionsLeadingToActivityFinals =
  let -- Create zero state using all places in the network for consistency
      allPlaces = S.toList $ places n
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
      -- Check if a transition corresponds to Activity Final
      isActivityFinalTransition t = case t of
        -- For normal petri nodes, check if the action leads to Activity Final
        NormalPetriNode {sourceNode = adNode} ->
          isActionNode adNode && Ad.label adNode `elem` actionsLeadingToActivityFinals
        -- For final petri nodes, check if it's an Activity Final
        FinalPetriNode {sourceNode = adNode} -> isActivityFinalNode adNode
        _ -> False
      g h xs = M.toList $
        M.fromList $ do
          (x, p) <- xs
          (t, y) <- successors n x
          Monad.guard $ h t
          -- If this is an Activity Final transition, immediately go to zero state
          let finalState = if isActivityFinalTransition t then zeroState else y
          return (finalState, t : p)
      f _ [] = []
      f [] xs =
        let next = g (`notElem` actions) xs               -- No further actions should be processed if no input is left
        in xs : f [] next
      f (a:as) xs =
        let consume = g (==a) xs                          -- Case: Next transition corresponds to input, therefore is processed and removed
            notConsume = g (`notElem` actions) xs         -- Case: Next transition is not an action, therefore is processed but not removed from input
        in union (f as consume) (f (a:as) notConsume)
  in f input [(start n, [])]

-- | Variant of levels' that manages visited states per path rather than globally.
-- This allows exploring cycles while preventing infinite loops within each path.
levelsWithCycles :: Ord s => Net s t -> [[(State s, [t])]]
levelsWithCycles n =
  let f [] = []
      f xs = xs' : f next'
        where
          xs' = map (\(x, p, _) -> (x, p)) xs
          next' = [ (y, t:p, S.insert y visited)
                  | (x, p, visited) <- xs
                  , (t, y) <- successors n x
                  , y `S.notMember` visited
                  ]
  in f [(start n, [], S.singleton (start n))]

{-|
Calculate the maximum distance between any two occurrences of the same action.
Returns Nothing if there are no repeated actions.

For example:

immediate repetition:

>>> actionRepetitionDistance ["A", "A"]
Just 0

1 action between repetitions:

>>> actionRepetitionDistance ["A", "B", "A"]
Just 1

2 actions between repetitions:

>>> actionRepetitionDistance ["A", "B", "C", "A"]
Just 2

no repetitions:

>>> actionRepetitionDistance ["A", "B", "C"]
Nothing
-}
actionRepetitionDistance :: [String] -> Maybe Int
actionRepetitionDistance actionSequence =
  let maxDistanceForAction action =
        let indices = [i | (i, a) <- zip [0..] actionSequence, a == action]
        in if length indices < 2
           then Nothing
           else Just (last indices - head indices - 1)
      distances = [d | action <- nubOrd actionSequence, Just d <- [maxDistanceForAction action]]
  in if null distances then Nothing else Just (maximum distances)
