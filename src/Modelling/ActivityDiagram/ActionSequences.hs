{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  generateActionSequence,
  isActivityFinalPetriNode,
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList, union, member, empty, toList)
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
  let actionsLeadingToActivityFinals = getActionsLeadingToActivityFinals diag
      tSeq = generateActionSequence' diag actionsLeadingToActivityFinals
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

-- Check if a PetriKey corresponds to an Activity Final node
isActivityFinalPetriNode :: PetriKey -> Bool
isActivityFinalPetriNode pk =
  case pk of
    FinalPetriNode _ srcNode -> isActivityFinalNode srcNode
    _ -> False

--Generate at one sequence of transitions to each final node
generateActionSequence' :: UMLActivityDiagram -> [Int] -> [PetriKey]
generateActionSequence' diag actionsLeadingToActivityFinals =
  let petri = fromPetriLike $ convertToPetriNet diag
      -- Use all places in the network to create the zero state for consistency
      allPlaces = S.toList $ places petri
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
      sequences = fromJust $ find (isJust . lookup zeroState) $ levelsAS petri actionsLeadingToActivityFinals
  in reverse $ fromJust $ lookup zeroState sequences

-- Modified version of levels' that handles Activity Final nodes
levelsAS :: Ord s => Net s PetriKey -> [Int] -> [[(State s, [PetriKey])]]
levelsAS n actionsLeadingToActivityFinals =
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


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let nameMap = map
        (\n -> (name n, Ad.label n))
        $ filter isActionNode $ nodes diag
      labels = mapMaybe (`lookup` nameMap) input
      petri = convertToPetriNet diag
      petriKeyMap = map
        (\k -> (Ad.label $ sourceNode k, k))
        $ filter isNormalPetriNode $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) labels
      actions = map snd $ filter (\(l,_) -> l `elem` map snd nameMap) petriKeyMap
      -- Get Action nodes that lead directly to Activity Final nodes
      actionsLeadingToActivityFinals = getActionsLeadingToActivityFinals diag
  in length input == length labels && validActionSequence' input' actions petri actionsLeadingToActivityFinals


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

validActionSequence'
  :: [PetriKey]
  -> [PetriKey]
  -> PetriLike Node PetriKey
  -> [Int]  -- Action node labels that lead to Activity Finals
  -> Bool
validActionSequence' input actions petri actionsLeadingToActivityFinals =
  let net = fromPetriLike petri
      -- Use all places in the network to create the zero state, not just the start state
      allPlaces = S.toList $ places net
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
  in any (isJust . lookup zeroState) (levelsCheckAS input actions net actionsLeadingToActivityFinals)


levelsCheckAS :: [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey -> [Int] -> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions n actionsLeadingToActivityFinals =
  let -- Create zero state using all places in the network for consistency
      allPlaces = S.toList $ places n
      zeroState = State $ M.fromList [(p, 0) | p <- allPlaces]
      -- Check if a transition corresponds to Activity Final by checking action nodes or FinalPetriNode
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
