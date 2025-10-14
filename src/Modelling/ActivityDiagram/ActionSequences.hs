{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  validActionSequenceWithPetri,
  generateActionSequence,
  generateActionSequenceWithPetri,
  generateActionSequenceWithPetriAndRepetition,
  terminatesSomeButNotAllFlowsWithPetri,
  hasActionRepetitionWithMinDistance
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList)
import qualified Data.Map as M (filter, map, keys, fromList, toList)

import Modelling.ActivityDiagram.Datatype (
  AdNode (..),
  UMLActivityDiagram (..),
  isActionNode
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

import Modelling.PetriNet.Reach.Step (levels', successors)

import Control.Monad (guard)
import Data.List (union)
import Data.Maybe(mapMaybe, isJust, fromJust)


fromPetriLike :: Ord a => PetriLike Node a -> Net a a
fromPetriLike petri =
  Net {
      places = S.fromList $ M.keys $ M.filter isPlaceNode $ allNodes petri,
      transitions = S.fromList $ M.keys $ M.filter isTransitionNode $ allNodes petri,
      connections = map (\(t,n) -> (M.keys $ flowIn n, t, M.keys $ flowOut n)) $ M.toList $ M.filter isTransitionNode $ allNodes petri,
      capacity = Unbounded,
      start = State {unState = M.map initial $ M.filter isPlaceNode $ allNodes petri}
  }

--Generate one valid action sequence to each of the final nodes
generateActionSequence :: UMLActivityDiagram -> [String]
generateActionSequence diag =
  generateActionSequenceWithPetri diag (convertToPetriNet diag)

-- | Generate one valid action sequence, using a pre-computed Petri net.
-- This version avoids re-computing the Petri net conversion
generateActionSequenceWithPetri :: UMLActivityDiagram -> PetriLike Node PetriKey -> [String]
generateActionSequenceWithPetri =
  generateActionSequenceWithPetriAndRepetition (-1)

-- | Generate one valid action sequence with optional repetition, using a pre-computed Petri net.
-- When minDistance >= 0, tries to generate sequences with action repetition where actions are
-- at least minDistance apart (0 = immediate repetition like [A,A], 1 = at least one action between like [A,B,A]).
-- When minDistance = -1, generates the shortest valid sequence without trying for repetition.
generateActionSequenceWithPetriAndRepetition :: Int -> UMLActivityDiagram -> PetriLike Node PetriKey -> [String]
generateActionSequenceWithPetriAndRepetition minDistance diag petri =
  let tSeq = generateActionSequence' minDistance petri
      tSeqLabels = map (Ad.label . sourceNode) $ filter isNormalPetriNode tSeq
      actions = map
        (\n -> (Ad.label n, name n))
        $ filter isActionNode $ nodes diag
  in mapMaybe (`lookup` actions) tSeqLabels

isNormalPetriNode :: PetriKey -> Bool
isNormalPetriNode pk =
  case pk of
    NormalPetriNode {} -> True
    _ -> False

-- | Check if a sequence of action names has repetition with at least the specified minimum distance
-- between repeated actions. For example:
-- minDistance = 0: [A,A,...] is valid (immediate repetition)
-- minDistance = 1: [A,B,A,...] is valid (at least 1 action between)
-- minDistance = 2: [A,B,C,A,...] is valid (at least 2 actions between)
hasActionRepetitionWithMinDistance :: Int -> [String] -> Bool
hasActionRepetitionWithMinDistance minDistance actionSequence =
  let -- Find all pairs of indices where the same action occurs
      indicesOf action = [i | (i, a) <- zip [0..] actionSequence, a == action]
      -- Check if any action has two occurrences with sufficient distance
      checkAction action =
        let indices = indicesOf action
        in any (\(i, j) -> j - i - 1 >= minDistance) [(i, j) | i <- indices, j <- indices, i < j]
  in any checkAction $ nub actionSequence
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

-- | Check if a sequence of PetriKeys (transitions) has repetition with minimum distance
-- Only considers NormalPetriNode transitions
hasTransitionRepetitionWithMinDistance :: Int -> [PetriKey] -> Bool
hasTransitionRepetitionWithMinDistance minDistance transitionSeq =
  let normalNodes = filter isNormalPetriNode transitionSeq
      labels = map (Ad.label . sourceNode) normalNodes
      -- Find all pairs of indices where the same label occurs
      indicesOf lbl = [i | (i, l) <- zip [0..] labels, l == lbl]
      -- Check if any label has two occurrences with sufficient distance
      checkLabel lbl =
        let indices = indicesOf lbl
        in any (\(i, j) -> j - i - 1 >= minDistance) [(i, j) | i <- indices, j <- indices, i < j]
  in any checkLabel $ nub labels
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

--Generate at one sequence of transitions to each final node
generateActionSequence' :: Int -> PetriLike Node PetriKey -> [PetriKey]
generateActionSequence' minDistance petriLike =
  let petri = fromPetriLike petriLike
      zeroState = State $ M.map (const 0) $ unState $ start petri
      allLevels = levels' petri
      levelsWithZeroState = filter (isJust . lookup zeroState) allLevels

      -- Extract all possible sequences to zero state
      allSequences = [reverse $ fromJust $ lookup zeroState level | level <- levelsWithZeroState]

      -- Find a sequence that matches the repetition requirement
      sequences = case allSequences of
        [] -> error "No path to zero state found"
        _ | minDistance >= 0 ->
            -- Try to find sequences with the required repetition pattern
            case filter (hasTransitionRepetitionWithMinDistance minDistance) allSequences of
              (bestSeq:_) -> bestSeq
              [] -> -- If none found with requirement, take longest available (best effort)
                    last allSequences
        _ -> -- No repetition requested, take shortest
            head allSequences
  in sequences


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let petri = convertToPetriNet diag
  in validActionSequenceWithPetri input diag petri

-- | Check if an action sequence is valid, using a pre-computed Petri net.
-- This version avoids re-computing the Petri net conversion
validActionSequenceWithPetri :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> Bool
validActionSequenceWithPetri input diag petri =
  let nameMap = map
        (\n -> (name n, Ad.label n))
        $ filter isActionNode $ nodes diag
      labels = mapMaybe (`lookup` nameMap) input
      petriKeyMap = map
        (\k -> (Ad.label $ sourceNode k, k))
        $ filter isNormalPetriNode $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) labels
      actions = map snd $ filter (\(l,_) -> l `elem` map snd nameMap) petriKeyMap
  in length input == length labels && validActionSequence' input' actions petri

-- | Check if an action sequence terminates some but not all flows, using a pre-computed Petri net.
-- This detects the case where a sequence terminates at least one flow
-- but doesn't reach the zero state (i.e., doesn't consume all tokens, leaving some flows active).
terminatesSomeButNotAllFlowsWithPetri :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> Bool
terminatesSomeButNotAllFlowsWithPetri input diag petri =
  let nameMap = map
        (\n -> (name n, Ad.label n))
        $ filter isActionNode $ nodes diag
      labels = mapMaybe (`lookup` nameMap) input
      petriKeyMap = map
        (\k -> (Ad.label $ sourceNode k, k))
        $ filter isNormalPetriNode $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) labels
      actions = map snd $ filter (\(l,_) -> l `elem` map snd nameMap) petriKeyMap
      net = fromPetriLike petri
      zeroState = State $ M.map (const 0) $ unState $ start net
      levels = levelsCheckAS input' actions net
      reachesZeroState = any (isJust . lookup zeroState) levels
      -- Check if any FinalPetriNode transition was fired (meaning a flow was terminated)
      finalNodeReached = any (any (\(_, path) -> any isFinalPetriNode path)) levels
  in length input == length labels && not reachesZeroState && finalNodeReached

-- | Check if a PetriKey represents a final node transition
isFinalPetriNode :: PetriKey -> Bool
isFinalPetriNode (FinalPetriNode {}) = True
isFinalPetriNode _ = False

validActionSequence'
  :: [PetriKey]
  -> [PetriKey]
  -> PetriLike Node PetriKey
  -> Bool
validActionSequence' input actions petri =
  let net = fromPetriLike petri
      zeroState = State $ M.map (const 0) $ unState $ start net
  in any (isJust . lookup zeroState) (levelsCheckAS input actions net)


levelsCheckAS :: [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey-> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions n =
  let g h xs = M.toList $
        M.fromList $ do
          (x, p) <- xs
          (t, y) <- successors n x
          guard $ h t
          return (y, t : p)
      f _ [] = []
      f [] xs =
        let next = g (`notElem` actions) xs               -- No further actions should be processed if no input is left
        in xs : f [] next
      f (a:as) xs =
        let consume = g (==a) xs                          -- Case: Next transition corresponds to input, therefore is processed and removed
            notConsume = g (`notElem` actions) xs         -- Case: Next transition is not an action, therefore is processed but not removed from input
        in union (f as consume) (f (a:as) notConsume)
  in f input [(start n, [])]
