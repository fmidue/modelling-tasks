{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-|
A module for parsing Petri Alloy instances into Haskell representations defined
by the 'Modelling.PetriNet.Types' module.
The instances contain valid and invalid Petri nets that is why these are parsed
into types as 'Net' which allow representing some invalid representations
of graphs which are similar to Petri nets.
-}
module Modelling.PetriNet.Parser (
  NoSingletonException (..),
  addCapacities,
  asSingleton,
  doubleSig,
  netToGr,
  netToGrWithCapacity,
  parseChange,
  parseNet,
  parseRenamedNet,
  singleSig,
  simpleNameMap, simpleRename,
  ) where

import qualified Modelling.PetriNet.Types         as PN (
  Net (nodes),
  )

import qualified Data.Bimap                       as BM (
  fromList, lookup,
  )
import qualified Data.Set                         as Set (
  Set, findMin, fromList, lookupMin, null, size, toList,
  )
import qualified Data.Map.Lazy                    as Map (
  alter,
  empty,
  findIndex,
  foldlWithKey',
  foldrWithKey,
  lookup,
  )
import Data.Maybe                       (fromMaybe)

import Modelling.Auxiliary.Common       (Object (Object, oName, oIndex), toMap)
import Modelling.PetriNet.Types (
  CapacityNode (..),
  Net (emptyNet, outFlow, alterFlow, alterNode, traverseNet),
  PetriChange (..),
  PetriLike (..),
  PetriNode (..),
  maybeCapacity,
  maybeInitial,
  )

import GHC.Num (integerFromInt)

import Control.Arrow                    (second)
import Control.Monad.Catch              (Exception, MonadThrow (throwM))
import Data.Bimap                       (Bimap)
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.Set                         (Set)
import Data.Map                         (Map)
import Data.Composition                 ((.:))
import Language.Alloy.Call (
  AlloyInstance,
  getDoubleAs,
  getSingleAs,
  getTripleAs,
  lookupSig,
  scoped,
  )

{-|
Parse a 'Net' graph from an 'AlloyInstance', using a certain node set accessor,
and given the instance's flow and token set names.
Return an already renamed Petri net, along with the renaming map.
-}
parseRenamedNet
  :: (MonadThrow m, Net p n)
  => (AlloyInstance -> m (Set Object))
  -> String
  -> String
  -> AlloyInstance
  -> m (p n String, Bimap Object String)
parseRenamedNet getNodes flowSetName tokenSetName inst = do
  petriLike <- parseNet getNodes flowSetName tokenSetName inst
  let nameMap = simpleNameMap petriLike
  net <- traverseNet (`BM.lookup` nameMap) petriLike
  return (net, nameMap)

{-|
Parse a 'Net' graph from an 'AlloyInstance', using a certain node set accessor,
and given the instance's flow and token set names.
-}
parseNet
  :: (MonadThrow m, Net p n)
  => (AlloyInstance -> m (Set Object))-- ^ how to get the relevant node set
  -> String                           -- ^ the name of the flow set
  -> String                           -- ^ the name of the token set
  -> AlloyInstance                    -- ^ the Petri net 'AlloyInstance'
  -> m (p n Object)
parseNet getNodes flowSetName tokenSetName inst = do
  nodes <- getNodes inst

  rawTokens <- doubleSig "this" "Places" tokenSetName inst
  let tokens = relToMap (second oIndex) rawTokens

  flow   <- tripleSig "this" "Nodes" flowSetName inst

  return
    . foldrFlip (\(x, y, z) -> alterFlow x (oIndex z) y) flow
    . foldrFlip
      (\x -> alterNode x $ Map.lookup x tokens >>= Set.lookupMin)
      nodes
    $ emptyNet
  where
    foldrFlip f = flip $ foldr f

addCapacities
  :: MonadThrow m
  => AlloyInstance
  -> PetriLike CapacityNode Object
  -> m (PetriLike CapacityNode Object)
addCapacities inst net = do
  nodes <- singleSig "this" "placesWithCapacity" "" inst

  rawCapacity <- doubleSig "this" "placesWithCapacity" "capacity" inst

  let capacities = relToMap (second (integerFromInt . oIndex)) rawCapacity

  return $ foldr (\x -> updateCapacity x $ Map.lookup x capacities >>= Set.lookupMin) net nodes

updateCapacity
    :: Object
    -> Maybe Integer
    -> PetriLike CapacityNode Object
    -> PetriLike CapacityNode Object
updateCapacity x y (PetriLike ns) =
    PetriLike $ Map.alter updateCapacity' x ns
    where
      updateCapacity' Nothing = Just $ CapacityPlace (fromMaybe undefined y) 0 Map.empty
      updateCapacity' (Just (CapacityPlace _ t o)) = Just $ CapacityPlace (fromMaybe undefined y) t o
      updateCapacity' (Just (CapacityTransition o)) = Just $ CapacityTransition o

relToMap :: (Ord b, Ord c) => (a -> (b, c)) -> Set a -> Map b (Set c)
relToMap f = toMap . Set.fromList . map f . Set.toList

{-|
Transform an 'Object' into a 'String' by replacing the prefix.
Returns 'Either':

 * an error message if no matching prefix was found
 * or the resulting 'String'
-}
simpleRename :: Object -> Either String String
simpleRename x = case oName x of
  "addedPlaces"      -> Right $ 'a':'S':y
  "addedTransitions" -> Right $ 'a':'T':y
  "givenPlaces"      -> Right $ 'S':y
  "givenTransitions" -> Right $ 'T':y
  _                  ->
    Left $ "simpleRename: Could not rename " ++ oName x ++ '$' : y
  where
    y = show (oIndex x)

{-|
Parses a 'PetriChange' given an 'AlloyInstance'.
On error a 'Left' error message will be returned.
-}
parseChange :: MonadThrow m => AlloyInstance -> m (PetriChange Object)
parseChange inst = do
  flow <- tripleSig "this" "Nodes" "flowChange" inst
  token <- doubleSig "this" "Places" "tokenChange" inst
  let tokenMap = relToMap (second oIndex) token
  tokenChange <- asSingleton `mapM` tokenMap
  let flowMap = relToMap tripleToOut flow
  let flowMap' = relToMap id <$> flowMap
  flowChange  <- mapM asSingleton `mapM` flowMap'
  return $ Change {..}
  where
    tripleToOut (x, y, z) = (x, (y, oIndex z))

data NoSingletonException
  = UnexpectedEmptySet
  | UnexpectedMultipleElements
  deriving Show

instance Exception NoSingletonException

{-|
Convert a singleton 'Set' into its single value.
Returns a 'Left' error message if the 'Set' is empty or contains more than one
single element.
-}
asSingleton :: MonadThrow m => Set b -> m b
asSingleton s
  | Set.null s
  = throwM UnexpectedEmptySet
  | Set.size s /= 1
  = throwM UnexpectedMultipleElements
  | otherwise
  = pure $ Set.findMin s

singleSig
  :: MonadThrow m
  => String
  -> String
  -> String
  -> AlloyInstance
  -> m (Set.Set Object)
singleSig st nd rd inst = do
  sig <- lookupSig (scoped st nd) inst
  getSingleAs rd (return .: Object) sig

doubleSig
  :: MonadThrow m
  => String
  -> String
  -> String
  -> AlloyInstance
  -> m (Set.Set (Object,Object))
doubleSig st nd rd inst = do
  sig <- lookupSig (scoped st nd) inst
  let obj = return .: Object
  getDoubleAs rd obj obj sig

tripleSig
  :: MonadThrow m
  => String
  -> String
  -> String
  -> AlloyInstance
  -> m (Set.Set (Object,Object,Object))
tripleSig st nd rd inst = do
  sig <- lookupSig (scoped st nd) inst
  let obj = return .: Object
  getTripleAs rd obj obj obj sig

{-|
Retrieve a simple naming map from a given 'Net'.
The newly created names for naming every 'PetriNode' of the 'Net' are unique
for each individually 'PetriNode'.
Furthermore, each place node's names prefix is a @s@, while each
transition node's name is preceded by a @t@.
These prefixes are followed by numbers starting at 1 and reaching to the number
of place nodes and transition nodes respectively.
-}
simpleNameMap :: (Net p n, Ord a) => p n a -> Bimap a String
simpleNameMap pl = BM.fromList . fst <$>
  Map.foldlWithKey'
  nameIncreasingly
  ([], (1 :: Integer, 1 :: Integer))
  $ PN.nodes pl
  where
    nameIncreasingly (ys, (p, t)) k x =
      let (k', p', t') = step x p t
      in ((k, k'):ys, (p', t'))
    step n p t
      | isPlaceNode n = ('s':show p, p + 1, t)
      | otherwise     = ('t':show t, p, t + 1)

{-|
Convert a 'Net' into a 'Gr' enabling to draw it using graphviz.
-}
netToGr
  :: (Monad m, Net p n, Ord a)
  => p n a
  -> m (Gr (a, Maybe Int) Int)
netToGr petriLike = do
  nodes <- Map.foldrWithKey convertNode (return []) $ PN.nodes petriLike
  let edges = Map.foldrWithKey convertTransition [] $ PN.nodes petriLike
  return $ mkGraph nodes edges
  where
    convertNode k x ns = do
      ns' <- ns
      return $ (indexOf k, (k, maybeInitial x)):ns'
    convertTransition k _ ns =
      Map.foldrWithKey (convertEdge k) ns $ outFlow k petriLike
    indexOf x = Map.findIndex x $ PN.nodes petriLike
    convertEdge source target flow rs =
      (indexOf source, indexOf target, flow) : rs

netToGrWithCapacity
  :: (Monad m, Net p CapacityNode, Ord a)
  => p CapacityNode a
  -> m (Gr (a, Maybe Int, Maybe Integer) Int)
netToGrWithCapacity petriLike = do
  nodes <- Map.foldrWithKey convertNode (return []) $ PN.nodes petriLike
  let edges = Map.foldrWithKey convertTransition [] $ PN.nodes petriLike
  return $ mkGraph nodes edges
  where
    convertNode k x ns = do
      ns' <- ns
      return $ (indexOf k, (k, maybeInitial x, maybeCapacity x)):ns'
    convertTransition k _ ns =
      Map.foldrWithKey (convertEdge k) ns $ outFlow k petriLike
    indexOf x = Map.findIndex x $ PN.nodes petriLike
    convertEdge source target flow rs =
      (indexOf source, indexOf target, flow) : rs
