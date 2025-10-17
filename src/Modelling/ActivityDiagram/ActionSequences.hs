{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  validActionSequenceWithPetri,
  generateActionSequence,
  generateActionSequenceWithPetri,
  generateActionSequenceWithPetriAndRepetition,
  terminatesSomeButNotAllFlowsWithPetri,
  actionRepetitionDistance
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList, singleton, insert, notMember)
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
import Data.Containers.ListUtils (nubOrd)
import Data.List (union, maximumBy)
import Data.Maybe(mapMaybe, isJust)
import Data.Ord (comparing)
import Data.Tuple (swap)


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
generateActionSequenceWithPetri diag petri =
  let actions = extractActionLookup diag
      transitionSequences = generateSequencesWithLevels levels' petri
  in transitionsToActionNamesWithLookup actions $ head transitionSequences

-- | Helper to convert transition sequences to action names using a pre-computed action lookup table
transitionsToActionNamesWithLookup :: [(Int, String)] -> [PetriKey] -> [String]
transitionsToActionNamesWithLookup actions transitionSequence =
  let transitionSequenceLabels = map (Ad.label . sourceNode) $ filter isNormalPetriNode transitionSequence
  in mapMaybe (`lookup` actions) transitionSequenceLabels

-- | Generate one valid action sequence with repetition, using a pre-computed Petri net.
-- This version allows cycle exploration to generate sequences with repeated actions.
-- Returns Nothing if no sequence with repetition can be found.
generateActionSequenceWithPetriAndRepetition :: UMLActivityDiagram -> PetriLike Node PetriKey -> Maybe [String]
generateActionSequenceWithPetriAndRepetition diag petri =
  let actions = extractActionLookup diag
      transitionSequences = generateSequencesWithLevels levelsWithCycles petri
      allActionSequences = map (transitionsToActionNamesWithLookup actions) transitionSequences
      sequencesWithDistances = [(seq', d) | seq' <- allActionSequences, Just d <- [actionRepetitionDistance seq']]
  in if null sequencesWithDistances
     then Nothing
     else Just $ fst $ maximumBy (comparing snd) sequencesWithDistances  -- Select sequence with maximum repetition distance

isNormalPetriNode :: PetriKey -> Bool
isNormalPetriNode pk =
  case pk of
    NormalPetriNode {} -> True
    _ -> False

-- | Extract action lookup table from diagram
extractActionLookup :: UMLActivityDiagram -> [(Int, String)]
extractActionLookup diag = map
  (\n -> (Ad.label n, name n))
  $ filter isActionNode $ nodes diag

-- | Calculate the maximum distance between any two occurrences of the same action.
-- Returns Nothing if there are no repeated actions.
-- For example:
-- [A,A] -> Just 0 (immediate repetition)
-- [A,B,A] -> Just 1 (1 action between repetitions)
-- [A,B,C,A] -> Just 2 (2 actions between repetitions)
-- [A,B,C] -> Nothing (no repetitions)
actionRepetitionDistance :: [String] -> Maybe Int
actionRepetitionDistance actionSequence =
  let maxDistanceForAction action =
        let indices = [i | (i, a) <- zip [0..] actionSequence, a == action]
        in if length indices < 2
           then Nothing
           else Just (maximum indices - minimum indices - 1)
      distances = [d | action <- nubOrd actionSequence, Just d <- [maxDistanceForAction action]]
  in if null distances then Nothing else Just (maximum distances)

-- | Helper to generate sequences using a specific levels function
generateSequencesWithLevels :: (Net PetriKey PetriKey -> [[(State PetriKey, [PetriKey])]]) -> PetriLike Node PetriKey -> [[PetriKey]]
generateSequencesWithLevels levelsFunction petriLike =
  let petri = fromPetriLike petriLike
      zeroState = State $ M.map (const 0) $ unState $ start petri
      allLevels = levelsFunction petri
  in [reverse p | level <- allLevels, (s, p) <- level, s == zeroState]

-- | Variant of levels' that manages visited states per path rather than globally
-- This allows exploring cycles while preventing infinite loops within each path
levelsWithCycles :: Ord s => Net s t -> [[(State s, [t])]]
levelsWithCycles n =
  let f xs
        | null xs = []
        | otherwise =
            xs' : f next'
              where
                xs' = map (\(x, p, _) -> (x, p)) xs
                next' = [ (y, t:p, S.insert y visited)
                        | (x, p, visited) <- xs
                        , (t, y) <- successors n x
                        , y `S.notMember` visited
                        ]
  in f [(start n, [], S.singleton (start n))]


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let petri = convertToPetriNet diag
  in validActionSequenceWithPetri input diag petri

-- | Check if an action sequence is valid, using a pre-computed Petri net.
-- This version avoids re-computing the Petri net conversion
validActionSequenceWithPetri :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> Bool
validActionSequenceWithPetri input diag petri =
  let nameMap = map swap (extractActionLookup diag)
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
  let nameMap = map swap (extractActionLookup diag)
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
