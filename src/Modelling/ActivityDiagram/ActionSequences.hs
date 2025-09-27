{-# LANGUAGE DuplicateRecordFields #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  generateActionSequence,
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList, empty, union, member)
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


-- Helper function to identify transitions that lead to Activity Final nodes
getTransitionsToActivityFinals :: UMLActivityDiagram -> PetriLike Node PetriKey -> [PetriKey]
getTransitionsToActivityFinals (UMLActivityDiagram adNodes adConnections) petri =
  let -- Find Activity Final nodes in the original diagram
      activityFinalLabels = [Ad.label node | node <- adNodes, isActivityFinalNode node]
      -- Find connections that lead to Activity Final nodes
      connectionsToActivityFinals = [conn | conn <- adConnections, to conn `elem` activityFinalLabels]
      -- Get the source node labels for these connections
      sourceLabels = map from connectionsToActivityFinals
      -- Find the corresponding PetriNet transitions
      petriKeys = M.keys $ allNodes petri
      transitionsToActivityFinals = [key | key <- petriKeys, 
                                           case key of 
                                             NormalPetriNode {sourceNode = srcNode} -> Ad.label srcNode `elem` sourceLabels
                                             _ -> False]
  in transitionsToActivityFinals

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
  let tSeq = generateActionSequence' diag
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
generateActionSequence' :: UMLActivityDiagram -> [PetriKey]
generateActionSequence' diag =
  let petri = fromPetriLike $ convertToPetriNet diag
      activityFinalTransitions = getTransitionsToActivityFinals diag (convertToPetriNet diag)
      zeroState = State $ M.map (const 0) $ unState $ start petri
      sequences = fromJust $ find (isJust . lookup zeroState) $ levelsAS activityFinalTransitions petri
  in reverse $ fromJust $ lookup zeroState sequences


-- Modified version of levels' that handles Activity Final transitions specially
levelsAS :: [PetriKey] -> Net PetriKey PetriKey -> [[(State PetriKey, [PetriKey])]]
levelsAS activityFinals n =
  let f _ [] = []
      f done xs =
        let done' = S.union done $ S.fromList $ map fst xs
            next = M.toList $ M.fromList [ (if t `elem` activityFinals 
                                              then State $ M.map (const 0) $ unState $ start n  -- Activity Final -> zero state
                                              else y, t:p) |
                (x,p) <- xs,
                (t,y) <- successors n x,
                not $ S.member (if t `elem` activityFinals 
                                  then State $ M.map (const 0) $ unState $ start n
                                  else y) done'
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
      -- Find transitions that lead to Activity Final nodes in the original diagram
      activityFinalTransitions = getTransitionsToActivityFinals diag petri
  in length input == length labels && validActionSequence' input' actions activityFinalTransitions petri


validActionSequence'
  :: [PetriKey]
  -> [PetriKey]
  -> [PetriKey]  -- Activity Final transitions
  -> PetriLike Node PetriKey
  -> Bool
validActionSequence' input actions activityFinals petri =
  let net = fromPetriLike petri
      zeroState = State $ M.map (const 0) $ unState $ start net
  in any (isJust . lookup zeroState) (levelsCheckAS input actions activityFinals net)


levelsCheckAS :: [PetriKey] -> [PetriKey] -> [PetriKey] -> Net PetriKey PetriKey-> [[(State PetriKey, [PetriKey])]]
levelsCheckAS input actions activityFinals n =
  let g h xs = M.toList $
        M.fromList $ do
          (x, p) <- xs
          (t, y) <- successors n x
          Monad.guard $ h t
          -- If this is an Activity Final transition, immediately return zero state
          if t `elem` activityFinals
            then return (State $ M.map (const 0) $ unState $ start n, t : p)
            else return (y, t : p)
      f _ [] = []
      f [] xs =
        let next = g (`notElem` actions) xs               -- No further actions should be processed if no input is left
        in xs : f [] next
      f (a:as) xs =
        let consume = g (==a) xs                          -- Case: Next transition corresponds to input, therefore is processed and removed
            notConsume = g (`notElem` actions) xs         -- Case: Next transition is not an action, therefore is processed but not removed from input
        in union (f as consume) (f (a:as) notConsume)
  in f input [(start n, [])]
