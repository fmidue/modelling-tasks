{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Modelling.ActivityDiagram.ActionSequences (
  validActionSequence,
  validActionSequenceWithPetri,
  generateActionSequence,
  generateActionSequencesWithPetri,
  generateActionSequenceWithPetriAndRepetition,
  actionRepetitionDistance,
  extractActionLookup,
  computeActionSequenceLevels,
  isFinalPetriNode
) where

import qualified Modelling.ActivityDiagram.Datatype as Ad (
  AdNode (label),
  )

import qualified Data.Set as S (fromList, singleton, insert, notMember)
import qualified Data.Map as M (filter, map, keys, fromList, toList)
import qualified Data.Bimap as BM (Bimap, fromList, lookupR, member)

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
import Control.Monad.Random (MonadRandom, uniform)
import Data.List (union)
import Data.List.Extra (nubOrd)
import Data.Maybe (mapMaybe, isJust)


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
  head $ generateActionSequencesWithPetri (convertToPetriNet diag) Nothing

-- | Generate valid action sequences, using a pre-computed Petri net.
-- The returned list may be infinite or some of its tails even diverge,
-- if no length constraints are passed.
generateActionSequencesWithPetri
  :: PetriLike Node PetriKey
  -> Maybe (Int, Int)  -- Optional (minLength, maxLength) constraints
  -> [[String]]
generateActionSequencesWithPetri =
  generateSequencesWithLevels levels'

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

isNormalPetriNode :: PetriKey -> Bool
isNormalPetriNode pk =
  case pk of
    NormalPetriNode {} -> True
    _ -> False

-- | Helper to generate sequences using a specific levels function
-- Now includes the action name conversion and length bounds filtering
generateSequencesWithLevels
  :: (Net PetriKey PetriKey -> [[(State PetriKey, [PetriKey])]])
  -> PetriLike Node PetriKey
  -> Maybe (Int, Int)  -- Optional (minLength, maxLength) constraints
  -> [[String]]
generateSequencesWithLevels levelsFunction petriLike maybeLengthBounds =
  let petri = fromPetriLike petriLike
      zeroState = State $ M.map (const 0) $ unState $ start petri
      relevantLevels = maybe id (\(minLength, maxLength) -> take (5 * maxLength) . drop minLength) maybeLengthBounds
                       $ levelsFunction petri
      convertAndFilterSequence transitionSequence =
        let actionSequence = mapMaybe (\case
                                          NormalPetriNode {sourceNode = AdActionNode {name = actionName}} -> Just actionName
                                          _ -> Nothing)
                             transitionSequence
            seqLength = length actionSequence
        in case maybeLengthBounds of
             Just (minLength, maxLength) | seqLength < minLength || seqLength > maxLength
               -> Nothing
             _ -> Just actionSequence
  in [ reverse a | level <- relevantLevels, (s, p) <- level, s == zeroState, Just a <- [convertAndFilterSequence p] ]


validActionSequence :: [String] -> UMLActivityDiagram -> Bool
validActionSequence input diag =
  let petri = convertToPetriNet diag
  in validActionSequenceWithPetri input (extractActionLookup diag) petri

-- | Check if an action sequence is valid, using a pre-computed Petri net.
validActionSequenceWithPetri :: [String] -> BM.Bimap Int String -> PetriLike Node PetriKey -> Bool
validActionSequenceWithPetri input actionLookup petri =
  let (levels, zeroState) = computeActionSequenceLevels input actionLookup petri
  in any (isJust . lookup zeroState) levels

-- | Common computation for action sequence validation
-- Returns (levels, zeroState) for checking sequence properties
computeActionSequenceLevels :: [String] -> BM.Bimap Int String -> PetriLike Node PetriKey -> ([[(State PetriKey, [PetriKey])]], State PetriKey)
computeActionSequenceLevels input actionLookup petri =
  let petriKeyMap = map
        (\k -> (Ad.label $ sourceNode k, k))
        $ filter isNormalPetriNode $ M.keys $ allNodes petri
      input' = mapMaybe (`lookup` petriKeyMap) (mapMaybe (`BM.lookupR` actionLookup) input)
      actions = map snd $ filter (\(l,_) -> l `BM.member` actionLookup) petriKeyMap
      net = fromPetriLike petri
      zeroState = State $ M.map (const 0) $ unState $ start net
      levels = levelsCheckAS input' actions net
  in (levels, zeroState)

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

-- | Variant of levels' that manages visited states per path rather than globally
-- This allows exploring cycles while preventing infinite loops within each path
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

-- | Extract action lookup table from diagram as a Bimap for bidirectional lookups
extractActionLookup :: UMLActivityDiagram -> BM.Bimap Int String
extractActionLookup diag = BM.fromList
  [ (Ad.label n, name n)
  | n <- nodes diag
  , isActionNode n
  ]

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
