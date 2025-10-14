{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  validActionSequenceWithPetri,
  generateActionSequence,
  generateActionSequenceWithPetri,
  reachesFinalNode
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
import Data.List (find, union)
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
generateActionSequenceWithPetri diag petri =
  let tSeq = generateActionSequence' petri
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

--Generate at one sequence of transitions to each final node
generateActionSequence' :: PetriLike Node PetriKey -> [PetriKey]
generateActionSequence' petriLike =
  let petri = fromPetriLike petriLike
      zeroState = State $ M.map (const 0) $ unState $ start petri
      sequences = fromJust $ find (isJust . lookup zeroState) $ levels' petri
  in reverse $ fromJust $ lookup zeroState sequences


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let petri = convertToPetriNet diag
  in validActionSequenceWithPetri input diag petri

-- | Common computation for action sequence validation
-- Returns (levels, zeroState) for checking sequence properties
computeActionSequenceLevels :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> ([[(State PetriKey, [PetriKey])]], State PetriKey)
computeActionSequenceLevels input diag petri =
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
  in (levels, zeroState)

-- | Check if an action sequence is valid, using a pre-computed Petri net.
-- This version avoids re-computing the Petri net conversion
validActionSequenceWithPetri :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> Bool
validActionSequenceWithPetri input diag petri =
  let (levels, zeroState) = computeActionSequenceLevels input diag petri
  in any (isJust . lookup zeroState) levels

-- | Check if a final node was reached in an action sequence execution.
-- This is used to provide feedback when a sequence terminates some but not all flows.
reachesFinalNode :: [String] -> UMLActivityDiagram -> PetriLike Node PetriKey -> Bool
reachesFinalNode input diag petri =
  let (levels, _) = computeActionSequenceLevels input diag petri
      -- Check if any FinalPetriNode transition was fired (meaning a flow was terminated)
      finalNodeReached = any (any (\(_, path) -> any isFinalPetriNode path)) levels
  in finalNodeReached

-- | Check if a PetriKey represents a final node transition
isFinalPetriNode :: PetriKey -> Bool
isFinalPetriNode (FinalPetriNode {}) = True
isFinalPetriNode _ = False


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
