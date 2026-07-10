{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Type.hs
-}
module Modelling.PetriNet.Reach.Type where

import qualified Data.Map                         as M (
  filter,
  findWithDefault,
  fromList,
  fromListWith,
  lookup,
  mapKeys,
  toList,
  )
import qualified Data.Set                         as S (
  fromList,
  isSubsetOf,
  map,
  )

import Modelling.Auxiliary.Common       (skipSpaces)

import Autolib.Hash                     (Hashable)
import Autolib.Reader.Class             (Reader (atomic_readerPrec))
import Autolib.ToDoc                    (ToDoc (toDocPrec), text)
import Data.Char                        (isSpace)
import Data.Data                        (Data)
import Data.List                        (intercalate)
import Data.Map                         (Map, (!))
import Data.Set                         (Set)
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  (<|>),
  char,
  many,
  many1,
  noneOf,
  optional,
  sepBy,
  skipMany,
  space,
  satisfy,
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

-- | Constraints on transition token behaviour in the net
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

-- | No transition behaviour constraints
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

newtype Place = Place String
  deriving anyclass Hashable
  deriving stock (Data, Eq, Generic, Ord, Read, Show)

newtype ShowPlace = ShowPlace Place
  deriving (Eq, Ord)

instance Show ShowPlace where
  show (ShowPlace (Place p)) = renderPetriName p

instance Reader Place where
  atomic_readerPrec = parsePlacePrec

instance ToDoc Place where
  toDocPrec _ = text . showPlace

showPlace :: Place -> String
showPlace = show . ShowPlace

placeFromNumber :: Int -> Place
placeFromNumber = Place . ('s':) . show

placesFromOneTo :: Int -> [Place]
placesFromOneTo = map placeFromNumber . enumFromTo 1

parsePlacePrec :: Int -> Parser Place
parsePlacePrec _ = do
  skipMany space
  Place <$> parsePetriName <* skipMany space

newtype Transition = Transition String
  deriving anyclass Hashable
  deriving stock (Data, Eq, Generic, Ord, Read, Show)

newtype ShowTransition = ShowTransition Transition
  deriving (Eq, Ord)

instance Show ShowTransition where
  show (ShowTransition (Transition t)) = renderPetriName t

instance Reader Transition where
  atomic_readerPrec = parseTransitionPrec

instance ToDoc Transition where
  toDocPrec _ = text . showTransition

showTransition :: Transition -> String
showTransition = show . ShowTransition

transitionFromNumber :: Int -> Transition
transitionFromNumber = Transition . ('t':) . show

transitionsFromOneTo :: Int -> [Transition]
transitionsFromOneTo = map transitionFromNumber . enumFromTo 1

parseTransitionPrec :: Int -> Parser Transition
parseTransitionPrec _ = do
  skipMany space
  Transition <$> parsePetriName <* skipMany space

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
    places = S.fromList [placeFromNumber 1, placeFromNumber 2, placeFromNumber 3, placeFromNumber 4],
    transitions = S.fromList [transitionFromNumber 1, transitionFromNumber 2, transitionFromNumber 3, transitionFromNumber 4],
    connections = [
        ([placeFromNumber 3, placeFromNumber 4], transitionFromNumber 1, [placeFromNumber 2]),
        ([placeFromNumber 4], transitionFromNumber 2, [placeFromNumber 3]),
        ([placeFromNumber 1], transitionFromNumber 3, [placeFromNumber 4]),
        ([placeFromNumber 2], transitionFromNumber 4, [placeFromNumber 1])
    ],
    capacity = Unbounded,
    start = State $ M.fromList
      [(placeFromNumber 1, 3), (placeFromNumber 2, 0), (placeFromNumber 3, 0), (placeFromNumber 4, 0)]
    },
   State $ M.fromList [(placeFromNumber 1, 0), (placeFromNumber 2, 0), (placeFromNumber 3, 1), (placeFromNumber 4, 0)]
  )

renderPetriName :: String -> String
renderPetriName name
  | all isUnquotedPetriNameCharacter name = name
  | otherwise = show name
  where
    isUnquotedPetriNameCharacter character =
      not (isSpace character) && character `notElem` ",()[]\""

parsePetriName :: Parser String
parsePetriName = parseQuotedPetriName <|> parseUnquotedPetriName

parseQuotedPetriName :: Parser String
parseQuotedPetriName = char '"' *> many (noneOf "\"") <* char '"'

parseUnquotedPetriName :: Parser String
parseUnquotedPetriName =
  many1 $ satisfy (\character -> not (isSpace character) && character `notElem` ",()[]")

-- | Check if a net has any isolated nodes (nodes with no connections)
hasIsolatedNodes :: (Ord s, Ord t) => Net s t -> Bool
hasIsolatedNodes (Net ps ts cs _ _) =
  let connectedPlaces = S.fromList $ concatMap (\(pre, _, post) -> pre ++ post) cs
      connectedTransitions = S.fromList $ map (\(_, t, _) -> t) cs
  in not (S.isSubsetOf ps connectedPlaces && S.isSubsetOf ts connectedTransitions)

-- | Determine the token behaviour of a connection
-- Returns: (consumed, produced)
connectionTokenBehavior :: Connection s t -> (Int, Int)
connectionTokenBehavior (prePlaces, _, postPlaces) =
  (length prePlaces, length postPlaces)

-- | Check if a net satisfies the given transition behaviour constraints
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
More specifically, a "fusable transition consuming" is a transition t where:
- t consumes (truly) from exactly one input place s, AND
- t is the only transition that consumes from s (except for trivial back-and-forth looping transitions)
-}
countFusableTransitionsConsuming :: Ord s => [([s], t, [s])] -> Int
countFusableTransitionsConsuming connections =
  length $ filter isFusableTransitionConsuming connections
  where
    isFusableTransitionConsuming (inputPlaces, _, outputPlaces) =
      case inputPlaces of
        [singlePlace] -> singlePlace `notElem` outputPlaces &&
                         null (tail (filter (\(pre, _, post) -> pre /= [singlePlace] || post /= [singlePlace]) (consumerMap ! singlePlace)))
        _ -> False
    consumerMap = M.fromListWith (++)
      [(place, [conn]) | conn@(pre, _, _) <- connections, place <- pre]

{- | Count transitions with exactly one output place which moreover is exclusively produced to by that transition.
More specifically, a "fusable transition producing" is a transition t where:
- t produces (truly) to exactly one place s, AND
- t is the only transition that produces to s (except for trivial back-and-forth looping transitions)
-}
countFusableTransitionsProducing :: Ord s => [([s], t, [s])] -> Int
countFusableTransitionsProducing connections =
  length $ filter isFusableTransitionProducing connections
  where
    isFusableTransitionProducing (inputPlaces, _, outputPlaces) =
      case outputPlaces of
        [singlePlace] -> singlePlace `notElem` inputPlaces &&
                         null (tail (filter (\(pre, _, post) -> pre /= [singlePlace] || post /= [singlePlace]) (producerMap ! singlePlace)))
        _ -> False
    producerMap = M.fromListWith (++)
      [(place, [conn]) | conn@(_, _, post) <- connections, place <- post]
