{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Type.hs
-}
module Modelling.PetriNet.Reach.Type where

import qualified Data.Map                         as M (
  elems,
  filter,
  findWithDefault,
  fromList,
  fromListWith,
  lookup,
  mapKeys,
  size,
  toList,
  )
import qualified Data.Set                         as S (
  fromList,
  isSubsetOf,
  map,
  )

import Modelling.Auxiliary.Common       (parseInt, skipSpaces)

import Autolib.Hash                     (Hashable)
import Autolib.Reader.Class             (Reader (atomic_readerPrec))
import Autolib.ToDoc                    (ToDoc (toDocPrec), text)
import Control.Monad                    (void)
import Data.Data                        (Data)
import Data.List                        (intercalate)
import Data.Map                         (Map, (!))
import Data.Set                         (Set)
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  char,
  optional,
  sepBy,
  skipMany,
  space,
  )

type Connection s t = ([s], t, [s])

newtype State s = State {unState :: Map s Int}
  deriving anyclass (Hashable, Reader, ToDoc)
  deriving stock (Data, Generic)

mapState :: Ord b => (a -> b) -> State a -> State b
mapState f (State x) = State { unState = M.mapKeys f x }

instance Ord s => Eq (State s) where
  State f == State g = M.filter (/= 0) f == M.filter (/= 0) g

instance Ord s => Ord (State s) where
  compare (State f) (State g) =
    compare (M.filter (/= 0) f) (M.filter (/= 0) g)

instance Show s => Show (State s) where
  show = show . M.toList . unState

instance (Ord s, Read s) => Read (State s) where
  readsPrec p xs = do
    (s, ys) <- readsPrec p xs
    return (State . M.fromList $ s, ys)

mark :: Ord s => State s -> s -> Int
mark (State f) s = M.findWithDefault 0 s f

data Capacity s
  = Unbounded
  | AllBounded Int
  | Bounded (Map s Int)
  deriving (Data, Eq, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

mapCapacity :: Ord a => (s -> a) -> Capacity s -> Capacity a
mapCapacity _ Unbounded      = Unbounded
mapCapacity _ (AllBounded x) = AllBounded x
mapCapacity f (Bounded m)    = Bounded $ M.mapKeys f m

-- | Constraints on transition token behavior in the net
data TransitionBehaviorConstraints = TransitionBehaviorConstraints {
  -- | Specify which token-changing transitions to allow.
  -- @Just LT@: allow only token-decreasing transitions (forbid increasing)
  -- @Just GT@: allow only token-increasing transitions (forbid decreasing)
  -- @Nothing@: allow both increasing and decreasing transitions
  -- Note: @Just EQ@ is rejected during config validation as meaningless
  -- (would only allow preserving transitions, conflicting with areNonPreserving)
  allowedTokenChanges :: Maybe Ordering,
  -- | Require exactly this many transitions to not be token-preserving.
  -- If @Nothing@, no restriction on number of non-preserving transitions.
  areNonPreserving :: Maybe Int
  }
  deriving (Data, Eq, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

-- | No transition behavior constraints
noTransitionBehaviorConstraints :: TransitionBehaviorConstraints
noTransitionBehaviorConstraints = TransitionBehaviorConstraints {
  allowedTokenChanges = Nothing,
  areNonPreserving = Nothing
  }

-- | Arrow density constraints for net generation
data ArrowDensityConstraints = ArrowDensityConstraints {
  -- | Constrain arrows entering each transition (from places)
  incomingArrowsPerTransition :: (Int, Maybe Int),
  -- | Constrain arrows leaving each transition (to places)
  outgoingArrowsPerTransition :: (Int, Maybe Int),
  -- | Constrain arrows entering each place (from transitions)
  incomingArrowsPerPlace :: (Int, Maybe Int),
  -- | Constrain arrows leaving each place (to transitions)
  outgoingArrowsPerPlace :: (Int, Maybe Int),
  -- | Global constraint on total arrows from places to transitions
  totalArrowsFromPlacesToTransitions :: (Int, Maybe Int),
  -- | Global constraint on total arrows from transitions to places
  totalArrowsFromTransitionsToPlaces :: (Int, Maybe Int)
  }
  deriving (Data, Eq, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

-- | Default arrow density constraints (no restrictions)
noArrowDensityConstraints :: ArrowDensityConstraints
noArrowDensityConstraints = ArrowDensityConstraints {
  incomingArrowsPerTransition = (0, Nothing),
  outgoingArrowsPerTransition = (0, Nothing),
  incomingArrowsPerPlace = (0, Nothing),
  outgoingArrowsPerPlace = (0, Nothing),
  totalArrowsFromPlacesToTransitions = (0, Nothing),
  totalArrowsFromTransitionsToPlaces = (0, Nothing)
  }

data Net s t = Net {
  places :: Set s,
  transitions :: Set t,
  connections :: [Connection s t],
  capacity :: Capacity s,
  start :: State s
  }
  deriving (Eq, Data, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

bimapNet :: (Ord a, Ord b) => (s -> a) -> (t -> b) -> Net s t -> Net a b
bimapNet f g x = Net {
  places      = S.map f (places x),
  transitions = S.map g (transitions x),
  connections = map bimapConnection $ connections x,
  capacity    = mapCapacity f $ capacity x,
  start       = mapState f $ start x
  }
  where
    bimapConnection (w, y, z) = (f <$> w, g y, f <$> z)

allNonNegative :: State a -> Bool
allNonNegative (State z) =
  all (\(_, v) -> v >= 0) (M.toList z)

conforms :: Ord k => Capacity k -> State k -> Bool
conforms cap (State z) = case cap of
  Unbounded -> True
  AllBounded b ->
    all (\(_, v) -> v <= b) (M.toList z)
  Bounded f -> all
    (\(k, v) -> case M.lookup k f of
        Nothing -> True
        Just b -> v <= b
    )
    (M.toList z)

newtype Place = Place Int
  deriving anyclass Hashable
  deriving newtype Enum
  deriving stock (Data, Eq, Generic, Ord, Read, Show)

newtype ShowPlace = ShowPlace Place
  deriving (Eq, Ord)

instance Show ShowPlace where
  show (ShowPlace (Place p)) = "s" ++ show p

instance Reader Place where
  atomic_readerPrec = parsePlacePrec

instance ToDoc Place where
  toDocPrec _ = text . showPlace

showPlace :: Place -> String
showPlace = show . ShowPlace

parsePlacePrec :: Int -> Parser Place
parsePlacePrec _ = do
  skipMany space
  void $ char 's'
  Place <$> parseInt <* skipMany space

newtype Transition = Transition Int
  deriving anyclass Hashable
  deriving newtype Enum
  deriving stock (Data, Eq, Generic, Ord, Read, Show)

newtype ShowTransition = ShowTransition Transition
  deriving (Eq, Ord)

instance Show ShowTransition where
  show (ShowTransition (Transition t)) = "t" ++ show t

instance Reader Transition where
  atomic_readerPrec = parseTransitionPrec

instance ToDoc Transition where
  toDocPrec _ = text . showTransition

showTransition :: Transition -> String
showTransition = show . ShowTransition

parseTransitionPrec :: Int -> Parser Transition
parseTransitionPrec _ = do
  skipMany space
  void $ char 't'
  Transition <$> parseInt <* skipMany space

newtype TransitionsList = TransitionsList {
  transitionsList :: [Transition]
  }
  deriving Generic

instance Show TransitionsList where
  show (TransitionsList ts) =
    '['
    : intercalate ", " (map showTransition ts)
    ++ "]"

instance Reader TransitionsList where
  atomic_readerPrec = parseTransitionsListPrec

instance ToDoc TransitionsList where
  toDocPrec _ = text . show

parseTransitionsListPrec :: Int -> Parser TransitionsList
parseTransitionsListPrec _ = do
  skipSpaces
  optional $ char '['
  ts <- parseTransitionPrec 0 `sepBy` optional (char ',')
  skipSpaces
  optional $ char ']' >> skipSpaces
  return $ TransitionsList ts

example :: (Net Place Transition, State Place)
example =
  (Net {
    places = S.fromList [Place 1, Place 2, Place 3, Place 4],
    transitions = S.fromList [Transition 1, Transition 2, Transition 3, Transition 4],
    connections = [
        ([Place 3, Place 4], Transition 1, [Place 2]),
        ([Place 4], Transition 2, [Place 3]),
        ([Place 1], Transition 3, [Place 4]),
        ([Place 2], Transition 4, [Place 1])
    ],
    capacity = Unbounded,
    start = State $ M.fromList
      [(Place 1, 3), (Place 2, 0), (Place 3, 0), (Place 4, 0)]
    },
   State $ M.fromList [(Place 1, 0), (Place 2, 0), (Place 3, 1), (Place 4, 0)]
  )

-- | Check if a net has any isolated nodes (nodes with no connections)
hasIsolatedNodes :: (Ord s, Ord t) => Net s t -> Bool
hasIsolatedNodes (Net ps ts cs _ _) =
  let connectedPlaces = S.fromList $ concatMap (\(pre, _, post) -> pre ++ post) cs
      connectedTransitions = S.fromList $ map (\(_, t, _) -> t) cs
  in not (S.isSubsetOf ps connectedPlaces && S.isSubsetOf ts connectedTransitions)

-- | Determine the token behavior of a connection
-- Returns: (consumed, produced)
connectionTokenBehavior :: Connection s t -> (Int, Int)
connectionTokenBehavior (prePlaces, _, postPlaces) =
  (length prePlaces, length postPlaces)

-- | Check if a net satisfies the given transition behavior constraints
satisfiesTransitionBehaviorConstraints
  :: Net s t
  -> TransitionBehaviorConstraints
  -> Bool
satisfiesTransitionBehaviorConstraints net TransitionBehaviorConstraints {..} =
  checkAllowedTypes && checkAreNonPreserving
  where
    checkAllowedTypes = case allowedTokenChanges of
      Nothing -> True
      Just LT -> all (uncurry (>=) . connectionTokenBehavior) (connections net)
      Just GT -> all (uncurry (<=) . connectionTokenBehavior) (connections net)
      Just EQ -> error "satisfiesTransitionBehaviorConstraints: Just EQ should be rejected already by config validation"
    checkAreNonPreserving = case areNonPreserving of
      Nothing -> True
      Just 0 -> all (uncurry (==) . connectionTokenBehavior) $ connections net
      Just expected ->
        let nonPreserving = length $ filter (uncurry (/=) . connectionTokenBehavior) $ connections net
        in nonPreserving == expected

{- | Count transitions with exactly one input place which moreover is exclusively consumed from by that transition.
A "fusable input node" is a transition t where:
- t consumes from exactly one input place s, AND
- t is the only transition that consumes from s (except for trivial back-and-forth looping transitions)
-}
countFusableInputNodes :: Ord s => Map t ([s], [s]) -> Int
countFusableInputNodes transitionPlacesMap =
  M.size $ M.filter isFusableInput transitionPlacesMap
  where
    isFusableInput (inputPlaces, outputPlaces) =
      case inputPlaces of
        [singlePlace] -> singlePlace `notElem` outputPlaces &&
                         null (tail (filter (\(pre, post) -> post /= [singlePlace] || any (/= singlePlace) pre) (consumerMap ! singlePlace)))
        _ -> False
    consumerMap = M.fromListWith (++)
      [(place, [places]) | places <- M.elems transitionPlacesMap, place <- fst places]

{- | Count transitions with exactly one output place which moreover is exclusively produced to by that transition.
A "fusable output node" is a transition t where:
- t produces to exactly one place s, AND
- t is the only transition that produces to s (except for trivial back-and-forth looping transitions)
-}
countFusableOutputNodes :: Ord s => Map t ([s], [s]) -> Int
countFusableOutputNodes transitionPlacesMap =
  M.size $ M.filter isFusableOutput transitionPlacesMap
  where
    isFusableOutput (inputPlaces, outputPlaces) =
      case outputPlaces of
        [singlePlace] -> singlePlace `notElem` inputPlaces &&
                         null (tail (filter (\(pre, post) -> pre /= [singlePlace] || any (/= singlePlace) post) (producerMap ! singlePlace)))
        _ -> False
    producerMap = M.fromListWith (++)
      [(place, [places]) | places <- M.elems transitionPlacesMap, place <- snd places]
