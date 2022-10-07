{-# LANGUAGE DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-|
This module provides types to represent Petri nets.

A Petri net is a mathematical modelling language.
It is used to describe distributed systems.
Another name for Petri net is place / transition (PT) net.

The 'Modelling.PetriNet.Types' module defines basic type class instances and
functions to work on and transform Petri net representations.
-}
module Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  Change,
  ChangeConfig (..),
  Concurrent (..),
  Conflict,
  ConflictConfig (..),
  DrawSettings (..),
  FindConcurrencyConfig (..),
  FindConflictConfig (..),
  Net (..),
  Node (..),
  Petri (..),
  PetriChange (..),
  PetriConflict (..),
  PetriConflict' (..),
  PetriLike (..),
  PetriMath (..),
  PetriNet,
  PetriNode (..),
  PickConcurrencyConfig (..),
  PickConflictConfig (..),
  SimpleNode (..),
  SimplePetriLike,
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  defaultFindConcurrencyConfig,
  defaultFindConflictConfig,
  defaultPickConcurrencyConfig,
  defaultPickConflictConfig,
  drawSettingsWithCommand,
  lAdvConfig,
  lAlloyConfig,
  lAtLeastActive,
  lBasicConfig,
  lChangeConfig,
  lConflictConfig,
  lConflictPlaces,
  lConflictTrans,
  lGraphLayout,
  lHidePlaceNames,
  lHideTransitionNames,
  lHideWeight1,
  lIsConnected,
  lMaxFlowOverall,
  lMaxFlowPerEdge,
  lMaxTokensOverall,
  lMaxTokensPerPlace,
  lMinFlowOverall,
  lMinTokensOverall,
  lPlaces,
  lPrintSolution,
  lTransitions,
  lUniqueConflictPlace,
  manyRandomDrawSettings,
  mapChange,
  maybeInitial,
  petriLikeToPetri,
  placeNames,
  randomDrawSettings,
  shuffleNames,
  transformNet,
  transitionNames,
  transitionPairShow,
  ) where

import qualified Modelling.PetriNet.Reach.Type    as Petri (Transition)

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map.Lazy                    as M (
  adjust,
  alter,
  delete,
  elems,
  empty,
  filter,
  foldrWithKey,
  insert,
  keys,
  keysSet,
  lookup,
  mapKeys,
  member,
  null,
  )
import qualified Data.Set                         as S (empty, union)

import Modelling.Auxiliary.Common       (lensRulesL, oneOf)
import Modelling.PetriNet.Reach.Type    (Place, ShowTransition (ShowTransition))

import Control.Lens                     (makeLensesWith)
import Control.Monad                    ((<=<))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random             (MonadRandom, RandT, RandomGen)
import Control.Monad.Trans              (MonadTrans(lift))
import Data.Bimap                       (Bimap)
import Data.GraphViz.Attributes.Complete (GraphvizCommand (..))
import Data.Map.Lazy                    (Map)
import Data.Maybe                       (fromMaybe)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bitraversable               (Bitraversable (bitraverse))

data AlloyConfig = AlloyConfig {
  maxInstances :: Maybe Integer,
  timeout      :: Maybe Int
  }
  deriving (Show, Read, Generic)

defaultAlloyConfig :: AlloyConfig
defaultAlloyConfig = AlloyConfig {
  maxInstances = Just 100,
  timeout      = Just 50000000
  }

{-|
A 'PetriChange' where nodes are labelled by strings.
-}
type Change = PetriChange String

{-|
A 'PetriChange' describes the changes on a 'PetriLike' graph by mapping 'PlaceNode's
to token changes and origins of an edge to a mapping from their targets to flow
changes.
-}
data PetriChange a = Change {
  -- | The token change 'Map': Mapping places to changes of their tokens.
  tokenChange :: Map a Int,
  -- | The flow change 'Map': Mapping source places to a mapping from target
  --   place to the flow change at the edge between source and target.
  flowChange  :: Map a (Map a Int)
  }
  deriving (Eq, Generic, Show)

{-|
This function acts like 'fmap' on other 'Functor's.

Note that 'Change' is not a true 'Functor' and thus 'mapChange' is not a true
'fmap' because an 'Ord' instance is required for 'Change's first type parameter
for 'mapChange' to work, furthermore (and that is the original reason),
'mapChange' uses 'M.mapKeys' internally in order to apply the mapping.
Thus, the user of 'mapChange' is responsible to ensure that the transformation
preserves uniqueness on all used keys.
-}
mapChange :: Ord b => (a -> b) -> PetriChange a -> PetriChange b
mapChange f (Change tc fc) =
  Change (M.mapKeys f tc) (M.mapKeys f $ M.mapKeys f <$> fc)

{-|
A 'PetriConflict' describes a conflict between two transitions.
It occurs when the number of tokens at the source place are not enough to fire
both transitions (both are having the same source place).
-}
data PetriConflict p t = Conflict {
  -- | The pair of transitions in conflict.
  conflictTrans :: (t, t),
  -- | The set of source nodes having not enough tokens to fire both transitions.
  conflictPlaces :: [p]
  }
  deriving (Generic, Read, Show)

makeLensesWith lensRulesL ''PetriConflict

{-|
A 'PetriConflict' where nodes are labelled by strings.
-}
type Conflict = PetriConflict Place Petri.Transition

newtype PetriConflict' x = PetriConflict' {
  toPetriConflict :: PetriConflict x x
  }
  deriving (Generic, Read, Show)

instance Functor PetriConflict' where
  fmap f = PetriConflict' . bimap f f . toPetriConflict

instance Foldable PetriConflict' where
  foldMap f = bifoldMap f f . toPetriConflict

instance Traversable PetriConflict' where
  traverse f = fmap PetriConflict' . bitraverse f f . toPetriConflict

instance Bifunctor PetriConflict where
  bimap f g (Conflict ts as) = Conflict (bimap g g ts) (f <$> as)

instance Bifoldable PetriConflict where
  bifoldMap f g (Conflict ts as) = foldMap f as <> bifoldMap g g ts

instance Bitraversable PetriConflict where
  bitraverse f g (Conflict ts as) = Conflict
    <$> bitraverse g g ts
    <*> traverse f as

newtype Concurrent a = Concurrent (a, a)
  deriving (Foldable, Functor, Generic, Read, Show, Traversable)

class Show (n String) => PetriNode n where
  initialTokens     :: n a -> Int

  {-|
  Whether the 'Node' is a 'PlaceNode'.
  -}
  isPlaceNode       :: n a -> Bool

  {-|
  Whether the 'PetriNode' is a 'TransitionNode'.
  -}
  isTransitionNode  :: n a -> Bool

  {-|
  This function acts like 'fmap' on other 'Functor's.

  Note that 'PetriNode' is not necessarily a true 'Functor' and thus 'mapNode'
  is not a true 'fmap' because an 'Ord' instance is required for 'Node's
  first type parameter for 'mapNode' to work,
  furthermore (and that is the original reason), 'mapNode' usually
  uses 'M.mapKeys' internally in order to apply the mapping. Thus, the user of
  'mapNode' is responsible to ensure that the transformation preserves uniqueness
  on all used keys.
  -}
  mapNode           :: Ord b => (a -> b) -> n a -> n b

  {-|
  This function acts like 'traverse' on 'Traversable'.

  Not that 'PetriNode' is not necessarily 'Traversable' itself as it requires
  an 'Ord' instance for the result type within the 'Applicative'
  of its first argument, the applicative lifting transformation function.
  This behaviour occurs, because the traversal changes the keys of the underlying
  'Map'.
  Transformations on this map require a specific traversal 'traverseKeyMap'.

  The user is responsible to ensure uniqueness of the keys after the traversal.
  Note, that the order of values could also change if the transformation is not
  order-preserving.
  -}
  traverseNode      :: (Applicative f, Ord b) => (a -> f b) -> n a -> f (n b)

{-|
A node is part of a Petri like graph (see 'PetriLike').
Each node stores its predecessor and successor nodes together with their weight
in the fields 'flowIn' and 'flowOut' respectively.
Additionally 'PlaceNode's have a value of initial tokens.
-}
data Node a =
  PlaceNode {
  -- | initial tokens of a 'PlaceNode'
  initial :: Int,
  -- | successor nodes
  flowIn  :: Map a Int,
  -- | predecessor nodes
  flowOut :: Map a Int
  } |
  TransitionNode {
  flowIn  :: Map a Int,
  flowOut :: Map a Int
  }
  deriving (Generic, Read, Show)

instance PetriNode Node where
  initialTokens = initial

  isPlaceNode PlaceNode {} = True
  isPlaceNode _            = False

  isTransitionNode TransitionNode {} = True
  isTransitionNode _                 = False

  mapNode f (PlaceNode s i o) =
    PlaceNode s (M.mapKeys f i) (M.mapKeys f o)
  mapNode f (TransitionNode i o) =
    TransitionNode (M.mapKeys f i) (M.mapKeys f o)

  traverseNode f (PlaceNode s i o) =
    PlaceNode s <$> traverseKeyMap f i <*> traverseKeyMap f o
  traverseNode f (TransitionNode i o) =
    TransitionNode <$> traverseKeyMap f i <*> traverseKeyMap f o

data SimpleNode a =
  SimplePlace {
  initial           :: Int,
  flowOut           :: Map a Int
  } |
  SimpleTransition {
  flowOut           :: Map a Int
  }
  deriving (Generic, Read, Show)

instance PetriNode SimpleNode where
  initialTokens = initial

  isPlaceNode SimplePlace {} = True
  isPlaceNode _         = False

  isTransitionNode SimpleTransition {} = True
  isTransitionNode _              = False

  mapNode f (SimplePlace s o) =
    SimplePlace s (M.mapKeys f o)
  mapNode f (SimpleTransition o) =
    SimpleTransition (M.mapKeys f o)

  traverseNode f (SimplePlace s o)    =
    SimplePlace s <$> traverseKeyMap f o
  traverseNode f (SimpleTransition o) =
    SimpleTransition <$> traverseKeyMap f o

{-|
Returns 'Just' the 'initial' tokens of the given node, if it is a place 'PetriNode',
otherwise it returns 'Nothing'.
-}
maybeInitial :: PetriNode n => n a -> Maybe Int
maybeInitial n
  | isPlaceNode n = Just $ initialTokens n
  | otherwise     = Nothing

{-|
A specific traversal for 'Map's changing the keys rather than values.
That is why, the result requires an 'Ord' instance.
It calls 'traverseKeyAndValueMap' but transforms only the keys.
-}
traverseKeyMap
  :: (Applicative f, Ord k2)
  => (k1 -> f k2) -- ^ transformation on keys
  -> Map k1 a
  -> f (Map k2 a)
traverseKeyMap f = traverseKeyAndValueMap f pure

{-|
A specific traversal for 'Map's transforming its keys and its values.
That is why, the result requires an 'Ord' instance on the resulting key values
type.
The traversal happens by inserting every changed key value pair into a new map.
-}
traverseKeyAndValueMap
  :: (Applicative f, Ord k2)
  => (k1 -> f k2) -- ^ transformation function on keys
  -> (a -> f b)   -- ^ transformation function on values
  -> Map k1 a
  -> f (Map k2 b)
traverseKeyAndValueMap f g =
  M.foldrWithKey insertApplicativeKeyValue (pure M.empty)
  where
    insertApplicativeKeyValue k x rs = M.insert <$> f k <*> g x <*> rs

class (PetriNode n, Show (p n String)) => Net p n where
  emptyNet :: p n a
  {-|
  Inserts 'flow' into the 'Net' by connecting the provided source and target
  by the given flow.
  If no 'PetriNode' for the given source or target exists within the 'Net'
  no 'flow' is added to the 'Net'
  If 'flow' between source and target exists already it is replaced.
  -}
  repsertFlow
    :: Ord a
    => a
    -- ^ source
    -> Int
    -- ^ the flow
    -> a
    -- ^ target
    -> p n a
    -> p n a

  {-|
  Inserts a 'PetriNode' into the 'Net' given the desired key,

   * a place node with the desired initial tokes if Just such are provided,
   * a transition node otherwise.

  If the desired key already exists the targeted 'PetriNode' is replaced
  without affecting preexisting 'flow'.
  (use 'deleteNode' first if you desire to clear related flow)
  -}
  repsertNode
    :: Ord a
    => a
    -- ^ node key
    -> Maybe Int
    -- ^ initial tokens
    -> p n a
    -> p n a

  {-|
  Removes the 'PetriNode' associated with the key and all connections going
  from or to the removed node.
  -}
  deleteNode        :: Ord a => a -> p n a -> p n a
  flow              :: Ord a => a -> a -> p n a -> Maybe Int
  nodes             :: Ord a => p n a -> Map a (n a)
  outFlow           :: Ord a => a -> p n a -> Map a Int
  mapNet            :: Ord b => (a -> b) -> p n a -> p n b
  traverseNet       :: (Applicative f, Ord b) => (a -> f b) -> p n a -> f (p n b)

updateNode
  :: (Map a Int -> Map b Int)
  -> (Map a Int -> Map b Int)
  -> Node a
  -> Node b
updateNode g h (PlaceNode t i o)    = PlaceNode t (g i) (h o)
updateNode g h (TransitionNode i o) = TransitionNode (g i) (h o)

adjustAll :: Ord a => (b -> b) -> Maybe [a] -> Map a b -> Map a b
adjustAll f ns m = foldr (M.adjust f) m $ concat ns

{-|
A Petri like graph consists of nodes which might have connections between each
other.

The 'PetriLike' graph is a valid Petri net only if

 * 'PlaceNode's are only successors of 'TransitionNode's
 * 'TransitionNode's are only successors of 'PlaceNode's
 * the initial marking is valid (i.e. all initial tokens are not negative)
 * every weight is greater than zero
-}
newtype PetriLike n a = PetriLike {
  -- | the 'Map' of all nodes the Petri net like graph is made of
  allNodes :: Map a (n a)
  } deriving (Generic, Read, Show)

instance Net PetriLike Node where
  emptyNet = PetriLike M.empty

  flow x y = (M.lookup y . flowOutN) <=< (M.lookup x . allNodes)

  nodes = allNodes

  deleteNode x (PetriLike ns) = PetriLike
    . M.delete x
    . adjustAll (updateNode id (M.delete x)) (M.keys . flowIn <$> n)
    . adjustAll (updateNode (M.delete x) id) (M.keys . flowOutN <$> n)
    $ ns
    where
      n = M.lookup x ns

  repsertFlow x f y = PetriLike
    . M.adjust (updateNode id (M.insert y f)) x
    . M.adjust (updateNode (M.insert x f) id) y
    . allNodes

  repsertNode x mt = PetriLike . M.alter alterNode x . allNodes
    where
      alterNode = Just . fromMaybe
        (maybe TransitionNode PlaceNode mt M.empty M.empty)

  outFlow x = maybe M.empty flowOutN . M.lookup x . allNodes

  mapNet = mapPetriLike
  traverseNet = traversePetriLike

flowOutN :: Node a -> Map a Int
flowOutN = flowOut

instance Net PetriLike SimpleNode where
  emptyNet = PetriLike M.empty

  flow x y = (M.lookup y . flowOutSN) <=< (M.lookup x . allNodes)

  nodes = allNodes

  deleteNode x = PetriLike . M.delete x . allNodes

  repsertFlow x f y = PetriLike
    . M.adjust (updateSimpleNode (M.insert y f)) x
    . allNodes
    where
      updateSimpleNode g (SimplePlace t o)    = SimplePlace t (g o)
      updateSimpleNode g (SimpleTransition o) = SimpleTransition (g o)

  repsertNode x mt = PetriLike . M.alter alterNode x . allNodes
    where
      alterNode = Just . fromMaybe
        (maybe SimpleTransition SimplePlace mt M.empty)

  outFlow x = maybe M.empty flowOutSN . M.lookup x . allNodes

  mapNet = mapPetriLike
  traverseNet = traversePetriLike

flowOutSN :: SimpleNode a -> Map a Int
flowOutSN = flowOut

type SimplePetriLike = PetriLike SimpleNode

{-|
A 'Functor' like 'fmap' on 'PetriLike'.

Note that 'PetriLike' is not a true 'Functor' as it requires the resulting type
to be an instance of 'Ord', because it uses 'M.mapKeys' in order to apply the
mapping on internal keys.

Thus, the user of 'mapPetriLike' is responsible to preserve uniqueness of values
(otherwise values might be lost after applying the mapping). Furthermore, if the
transformation is not order-preserving, the order of keys within 'Map's might
be changed.
-}
mapPetriLike
  :: (Ord b, PetriNode n)
  => (a -> b)
  -> PetriLike n a
  -> PetriLike n b
mapPetriLike f x = PetriLike $ M.mapKeys f $ mapNode f <$> allNodes x

{-|
A 'Traversable' like 'traverse' on 'PetriLike'.

Note that 'PetriLike' is not a true 'Traversable' as it requires the resulting
type to be an instance of 'Ord', because it uses 'traverseKeyAndValueMap' which
requires this constraint due to changing keys of 'Map's.

Thus, the user is responsible to preserve uniqueness of keys.
Furthermore, the order of keys might be changed if the transformation is not
order-preserving.
-}
traversePetriLike
  :: (Applicative f, Ord b, PetriNode n)
  => (a -> f b)
  -> PetriLike n a
  -> f (PetriLike n b)
traversePetriLike f x =
  PetriLike <$> traverseKeyAndValueMap f (traverseNode f) (allNodes x)

transitionNames :: (Net p n, Ord k) => p n k -> [k]
transitionNames = M.keys . M.filter isTransitionNode . nodes

placeNames :: (Net p n, Ord k) => p n k -> [k]
placeNames = M.keys . M.filter isPlaceNode . nodes

shuffleNames
  :: (MonadThrow m, Net p n, Ord a, RandomGen g)
  => p n a
  -> RandT g m (p n a, Bimap a a)
shuffleNames pl = do
  let ts = transitionNames pl
      ps = placeNames pl
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  lift $ (,mapping) <$> traverseNet (`BM.lookup` mapping) pl

transformNet
  :: (Net p n, Net p' n', Ord a)
  => p n a
  -> p' n' a
transformNet ns =
  M.foldrWithKey fromSimpleNode emptyNet $ nodes ns
  where
    insertFlows k xs = M.foldrWithKey (flip (repsertFlow k)) xs (outFlow k ns)
    fromSimpleNode k n = insertFlows k . repsertNode k (maybeInitial n)

{-|
Transform a 'PetriLike' graph into a 'Petri' net.
It first checks if the given Petri net like graph is indeed a valid Petri net
(see also 'PetriLike'),

* if it is, the Petri net like graph is transformed into a Petri net by
  eliminating references to names of places and transitions at all.
  Instead 'initialMarking' is given by a list (where each position represents
  different places) and transitions ('trans') are given by a lists of token
  change (where, again, each position represents a different place, but the same
  index within 'initialMarking' and 'trans' represents the same place).
* if it is not, a message is returned indicating the reason why the given
  Petri net like graph is not a valid Petri net.
-}
petriLikeToPetri :: Ord a => PetriLike Node a -> Either String Petri
petriLikeToPetri p = do
  isValid
  return $ Petri {
    initialMarking = initialTokens <$> M.elems ps,
    trans          =
      foldr ((:) . toChangeTuple) [] ts
    }
  where
    ps = M.filter isPlaceNode $ allNodes p
    ts = M.filter isTransitionNode $ allNodes p
    isValid
      | not (M.null $ M.filter ((< 0) . initialTokens) ps)
      = Left "Invalid Petri net: place with negative token number"
      | any (`M.member` ts) (allRelatedNodes ts)
      = Left "related nodes of TransitionNodes contain TranisitionNodes"
      | any (`M.member` ps) (allRelatedNodes ps)
      = Left "related nodes of PlaceNodes contain PlaceNodes"
      | any (any (<= 0) . flowIn) ts
      = Left "flow to a transition is zero or less"
      | any (any (<= 0) . flowOutN) ts
      = Left "flow from a transition is zero or less"
      | otherwise
      = return ()
    toChangeTuple n = (toFlowList flowIn n, toFlowList flowOutN n)
    toFlowList f n = M.foldrWithKey
      (\k _ xs -> fromMaybe 0 (M.lookup k $ f n) : xs)
      []
      ps
    relatedNodes n = M.keysSet (flowIn n) `S.union` M.keysSet (flowOutN n)
    allRelatedNodes = foldr
      (S.union . relatedNodes)
      S.empty

type Marking = [Int]
type Transition = (Marking,Marking)

{-|
Stores a mathematical representation of a Petri net based on a five tuple.
-}
data PetriMath a = PetriMath {
  -- | the five tuple itself
  netMath            :: a,
  -- | the places (the first element of the five tuple)
  placesMath         :: a,
  -- | the transitions (the second element of the five tuple)
  transitionsMath    :: a,
  {- | the token change of each transition
       (the third and fourth element of the five tuple) -}
  tokenChangeMath    :: [(a, a)],
  -- | the initial marking (the fifth element of the five tuple)
  initialMarkingMath :: a,
  -- | the order of places used for notation of token changes ('tokenChangeMath')
  placeOrderMath     :: Maybe a
  } deriving (Foldable, Functor, Generic, Read, Show, Traversable)

data Petri = Petri
  { initialMarking :: Marking
  , trans :: [Transition]
  } deriving (Eq, Generic, Read, Show)

data BasicConfig = BasicConfig
  { places :: Int
  , transitions :: Int
  , atLeastActive :: Int
  , minTokensOverall :: Int
  , maxTokensOverall :: Int
  , maxTokensPerPlace :: Int
  , minFlowOverall :: Int
  , maxFlowOverall :: Int
  , maxFlowPerEdge :: Int
  , isConnected :: Maybe Bool
  , graphLayout :: [GraphvizCommand]
  , hideWeight1 :: Bool
  , hidePlaceNames :: Bool
  , hideTransitionNames :: Bool
  } deriving (Generic, Read, Show)

makeLensesWith lensRulesL ''BasicConfig

defaultBasicConfig :: BasicConfig
defaultBasicConfig = BasicConfig
  { places = 4
  , transitions = 3
  , atLeastActive = 1
  , minTokensOverall = 2
  , maxTokensOverall = 7
  , maxTokensPerPlace = 2
  , minFlowOverall = 6
  , maxFlowOverall = 12
  , maxFlowPerEdge = 2
  , isConnected = Just True
  , graphLayout = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork]
  , hideWeight1 = True
  , hidePlaceNames = False
  , hideTransitionNames = False
  }

data AdvConfig = AdvConfig
  { presenceOfSelfLoops :: Maybe Bool
  , presenceOfSinkTransitions :: Maybe Bool
  , presenceOfSourceTransitions :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultAdvConfig :: AdvConfig
defaultAdvConfig = AdvConfig
  { presenceOfSelfLoops = Just False
  , presenceOfSinkTransitions = Just False
  , presenceOfSourceTransitions = Just False
  }

data ChangeConfig = ChangeConfig
  { tokenChangeOverall :: Int
  , maxTokenChangePerPlace :: Int
  , flowChangeOverall :: Int
  , maxFlowChangePerEdge :: Int
  } deriving (Generic, Read, Show)

defaultChangeConfig :: ChangeConfig
defaultChangeConfig = ChangeConfig
  { tokenChangeOverall = 2
  , maxTokenChangePerPlace = 1
  , flowChangeOverall = 2
  , maxFlowChangePerEdge = 1
  }

data ConflictConfig = ConflictConfig {
  -- | to enforce (no) extra places being common preconditions
  -- (but not in conflict) for the transitions in conflict
  addConflictCommonPreconditions        :: Maybe Bool,
  -- | to enforce the (non-)existence of conflict distractors
  withConflictDistractors               :: Maybe Bool,
  -- | to enforce the (non-)existence of more common preconditions
  -- than places in conflict for at least one distractor
  conflictDistractorAddExtraPreconditions :: Maybe Bool,
  -- | to enforce that at least one distractor looks conflict like
  conflictDistractorOnlyConflictLike    :: Bool,
  -- | to enforce that at least one distractor looks concurrent like
  conflictDistractorOnlyConcurrentLike  :: Bool
  }
  deriving (Generic, Read, Show)

defaultConflictConfig :: ConflictConfig
defaultConflictConfig = ConflictConfig {
  addConflictCommonPreconditions        = Nothing,
  withConflictDistractors               = Nothing,
  conflictDistractorAddExtraPreconditions = Nothing,
  conflictDistractorOnlyConflictLike    = False,
  conflictDistractorOnlyConcurrentLike  = False
  }

data FindConflictConfig = FindConflictConfig
  { basicConfig :: BasicConfig
  , advConfig :: AdvConfig
  , changeConfig :: ChangeConfig
  , conflictConfig :: ConflictConfig
  , printSolution :: Bool
  , uniqueConflictPlace :: Maybe Bool
  , alloyConfig  :: AlloyConfig
  } deriving (Generic, Read, Show)

makeLensesWith lensRulesL ''FindConflictConfig

defaultFindConflictConfig :: FindConflictConfig
defaultFindConflictConfig = FindConflictConfig
  { basicConfig = defaultBasicConfig{ atLeastActive = 3, hidePlaceNames = True }
  , advConfig = defaultAdvConfig{ presenceOfSourceTransitions = Nothing }
  , changeConfig = defaultChangeConfig
  , conflictConfig = defaultConflictConfig
  , printSolution = False
  , uniqueConflictPlace = Just True
  , alloyConfig  = defaultAlloyConfig
  }

data PickConflictConfig = PickConflictConfig
  { basicConfig :: BasicConfig
  , changeConfig :: ChangeConfig
  , conflictConfig :: ConflictConfig
  , printSolution :: Bool
  , prohibitSourceTransitions :: Bool
  , uniqueConflictPlace :: Maybe Bool
  , useDifferentGraphLayouts :: Bool
  , alloyConfig  :: AlloyConfig
  } deriving (Generic, Read, Show)

defaultPickConflictConfig :: PickConflictConfig
defaultPickConflictConfig = PickConflictConfig
  { basicConfig = defaultBasicConfig{ atLeastActive = 2, hidePlaceNames = True, hideTransitionNames = True }
  , changeConfig = defaultChangeConfig
  , conflictConfig = defaultConflictConfig
  , printSolution = False
  , prohibitSourceTransitions = False
  , uniqueConflictPlace = Nothing
  , useDifferentGraphLayouts = False
  , alloyConfig  = defaultAlloyConfig
  }

data FindConcurrencyConfig = FindConcurrencyConfig
  { basicConfig :: BasicConfig
  , advConfig :: AdvConfig
  , changeConfig :: ChangeConfig
  , printSolution :: Bool
  , alloyConfig  :: AlloyConfig
  } deriving (Generic, Read, Show)

defaultFindConcurrencyConfig :: FindConcurrencyConfig
defaultFindConcurrencyConfig = FindConcurrencyConfig
  { basicConfig = defaultBasicConfig{ atLeastActive = 3, hidePlaceNames = True }
  , advConfig = defaultAdvConfig{ presenceOfSourceTransitions = Nothing }
  , changeConfig = defaultChangeConfig
  , printSolution = False
  , alloyConfig  = defaultAlloyConfig
  }

data PickConcurrencyConfig = PickConcurrencyConfig
  { basicConfig :: BasicConfig
  , changeConfig :: ChangeConfig
  , printSolution :: Bool
  , prohibitSourceTransitions :: Bool
  , useDifferentGraphLayouts :: Bool
  , alloyConfig  :: AlloyConfig
  } deriving (Generic, Read, Show)

defaultPickConcurrencyConfig :: PickConcurrencyConfig
defaultPickConcurrencyConfig = PickConcurrencyConfig
  { basicConfig = defaultBasicConfig{ atLeastActive = 2, hidePlaceNames = True, hideTransitionNames = True }
  , changeConfig = defaultChangeConfig
  , printSolution = False
  , prohibitSourceTransitions = False
  , useDifferentGraphLayouts = False
  , alloyConfig  = defaultAlloyConfig
  }

data DrawSettings = DrawSettings {
  withPlaceNames       :: Bool,
  withTransitionNames  :: Bool,
  with1Weights         :: Bool,
  withGraphvizCommand  :: GraphvizCommand
  } deriving (Generic, Read, Show)

type PetriNet n = (PetriLike n String, DrawSettings)

drawSettingsWithCommand :: BasicConfig -> GraphvizCommand -> DrawSettings
drawSettingsWithCommand config c = DrawSettings {
  withPlaceNames = not $ hidePlaceNames config,
  withTransitionNames = not $ hideTransitionNames config,
  with1Weights = not $ hideWeight1 config,
  withGraphvizCommand = c
  }

{-|
Provides a 'DrawSetting' by using 'drawSettingsWithCommand' and randomly picking
one of the provided 'graphLayout's.
-}
randomDrawSettings :: MonadRandom m => BasicConfig -> m DrawSettings
randomDrawSettings config =
  drawSettingsWithCommand config <$> oneOf (graphLayout config)

{-|
Provides a list of 'DrawSettings' with as many entries as specified by randomly
picking while ensuring as few repetitions of provided 'graphLayout's as possible.
-}
manyRandomDrawSettings
  :: MonadRandom m
  => BasicConfig
  -- ^ providing layouts to pick from
  -> Int
  -- ^ how many entries to return
  -> m [DrawSettings]
manyRandomDrawSettings config n = map (drawSettingsWithCommand config) <$> do
  let gls = graphLayout config
  gls' <- shuffleM gls
  shuffleM $ take n $ concat $ repeat gls'

transitionPairShow
  :: (Petri.Transition, Petri.Transition)
  -> (ShowTransition, ShowTransition)
transitionPairShow = bimap ShowTransition ShowTransition

checkBasicConfig :: BasicConfig -> Maybe String
checkBasicConfig BasicConfig{
  atLeastActive,
  graphLayout,
  maxFlowOverall,
  maxFlowPerEdge,
  maxTokensOverall,
  maxTokensPerPlace,
  minFlowOverall,
  minTokensOverall,
  places,
  transitions
  }
 | places <= 0
  = Just "The number of places must be positive."
 | places > 8
  = Just "Cannot deal with more than 8 places."
 | transitions <= 0
  = Just "The number of transitions must be positive."
 | transitions > 8
  = Just "Cannot deal with more than 8 transitions."
 | atLeastActive < 0
  = Just "The parameter 'atLeastActive' must be non-negative."
 | atLeastActive > transitions
  = Just "There cannot be more active transitions than there are transitions."
 | minTokensOverall < 0
  = Just "The parameter 'minTokensOverall' must be non-negative."
 | maxTokensOverall < minTokensOverall
  = Just "The parameter 'minTokensOverall' must not be larger than 'maxTokensOverall'."
 | maxTokensPerPlace < 0
  = Just "The parameter 'maxTokensPerPlace' must be non-negative."
 | maxTokensPerPlace > maxTokensOverall
  = Just "The parameter 'maxTokensPerPlace' must not be larger than 'maxTokensOverall'."
 | maxTokensOverall > places * maxTokensPerPlace
  = Just "The parameter 'maxTokensOverall' is set unreasonably high, given the per-place parameter."
 | minFlowOverall < 0
  = Just "The parameter 'minFlowOverall' must be non-negative."
 | maxFlowOverall < minFlowOverall
  = Just "The parameter 'minFlowOverall' must not be larger than 'maxFlowOverall'."
 | maxFlowPerEdge <= 0
  = Just "The parameter 'maxFlowPerEdge' must be positive."
 | maxFlowOverall < maxFlowPerEdge
  = Just "The parameter 'maxFlowPerEdge' must not be larger than 'maxFlowOverall'."
 | maxFlowOverall > 2 * places * transitions * maxFlowPerEdge
  = Just "The parameter 'maxFlowOverall' is set unreasonably high, given the other parameters."
 | transitions + places > 1 + minFlowOverall
  = Just "The number of transitions and places exceeds 'minFlowOverall' too much to create a connected net."
 | null graphLayout
 = Just "At least one graph layout needs to be provided."
 | otherwise
  = Nothing

checkChangeConfig :: BasicConfig -> ChangeConfig -> Maybe String
checkChangeConfig
  BasicConfig {
    places,
    transitions,
    maxTokensOverall,
    maxTokensPerPlace,
    maxFlowOverall,
    maxFlowPerEdge
    }
  ChangeConfig {
    tokenChangeOverall,
    flowChangeOverall,
    maxFlowChangePerEdge,
    maxTokenChangePerPlace
    }
 | tokenChangeOverall < 0
  = Just "The parameter 'tokenChangeOverall' must be non-negative."
 | maxTokenChangePerPlace < 0
  = Just "The parameter 'maxTokenChangePerPlace' must be non-negative."
 | maxTokenChangePerPlace > tokenChangeOverall
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'tokenChangeOverall'."
 | maxTokenChangePerPlace > maxTokensPerPlace
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'maxTokensPerPlace'."
 | tokenChangeOverall > 2 * maxTokensOverall
  = Just "The parameter 'tokenChangeOverall' is set unreasonably high, given the maximal tokens overall."
 | maxTokenChangePerPlace * places < tokenChangeOverall
  = Just "The parameter 'tokenChangeOverall' is set unreasonably high, given the per-place parameter."
 | flowChangeOverall < 0
  = Just "The parameter 'flowChangeOverall' must be non-negative."
 | maxFlowChangePerEdge < 0
  = Just "The parameter 'maxFlowChangePerEdge' must be non-negative."
 | maxFlowChangePerEdge > flowChangeOverall
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'flowChangeOverall'."
 | maxFlowChangePerEdge > maxFlowPerEdge
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'maxFlowPerEdge'."
 | flowChangeOverall > 2 * maxFlowOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonable high, given the maximal flow overall."
 | 2 * places * transitions * maxFlowChangePerEdge < flowChangeOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonably high, given the other parameters."
 | otherwise
  = Nothing

checkGraphLayouts :: Bool -> Int -> BasicConfig -> Maybe String
checkGraphLayouts useDifferent wrongInstances bc
  | useDifferent && length (graphLayout bc) <= wrongInstances
  = Just "The parameter 'graphLayout' has to contain more entries than the number of 'wrongInstances' if 'useDifferentGraphLayouts' is set."
  | otherwise
  = Nothing
