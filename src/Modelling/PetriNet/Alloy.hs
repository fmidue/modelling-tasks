{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# Language DuplicateRecordFields #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.Alloy (
  TaskGenerationException (..),
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  connected,
  defaultConstraints,
  isolated,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitWidth,
  petriScopeMaxSeq,
  signatures,
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  ) where

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Modelling.Auxiliary.Common (
  TaskGenerationException (NoInstanceAvailable),
  Object (Object),
  upperFirst,
  )
import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig,
  BasicConfig (..),
  ChangeConfig (..),
  )

import qualified Modelling.PetriNet.Types         as T (
  AlloyConfig (maxInstances, timeout)
  )

import Control.Monad                    (when)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.Random (
  RandT,
  Random (randomR),
  RandomGen,
  liftRandT,
  )
import Data.Composition                 ((.:))
import Data.FileEmbed                   (embedStringFile)
import Data.List                        (intercalate)
import Data.Set                         (Set)
import Data.String.Interpolate          (i)
import Language.Alloy.Call (
  AlloyInstance,
  getSingleAs,
  lookupSig,
  unscoped,
  )

petriScopeBitWidth :: BasicConfig -> Int
petriScopeBitWidth BasicConfig
 { flowOverall, places, tokensOverall, transitions } =
  floor
     (2 + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral)
       (maximum [snd flowOverall, snd tokensOverall, places, transitions])
     )

petriScopeMaxSeq :: BasicConfig -> Int
petriScopeMaxSeq BasicConfig{places,transitions} = places+transitions

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "alloy/petri/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "alloy/petri/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "alloy/petri/Helpers.als")

modulePetriConcepts :: String
modulePetriConcepts = removeLines 5 $(embedStringFile "alloy/petri/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 5 $(embedStringFile "alloy/petri/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

{-|
A set of constraints enforcing settings of 'BasicConfig'.
(Besides 'defaultConstraints')
-}
compBasicConstraints
  :: String
  -- ^ The name of the Alloy variable for the set of activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
compBasicConstraints = enforceConstraints False

{-|
A set of constraints enforcing settings of 'BasicConfig' for the net under
default conditions.
-}
defaultConstraints
  :: String
  -- ^ The name of the Alloy variable for the set of default activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
defaultConstraints = enforceConstraints True

enforceConstraints
  :: Bool
  -- ^ If to generate constraints under default conditions.
  -> String
  -- ^ The name of the Alloy variable for the set of activated Transitions.
  -> BasicConfig
  -- ^ the configuration to enforce.
  -> String
enforceConstraints underDefault activated BasicConfig {
  atLeastActive,
  isConnected,
  flowOverall,
  maxFlowPerEdge,
  maxTokensPerPlace,
  tokensOverall
  } = [i|
  let t = (sum p : #{places} | p.#{tokens}) |
    #{fst tokensOverall} =< t and t =< #{snd tokensOverall}
  all p : #{places} | p.#{tokens} =< #{maxTokensPerPlace}
  all w : #{nodes}.#{flow}[#{nodes}] | w =< #{maxFlowPerEdge}
  let theFlow = (sum f, t : #{nodes} | f.#{flow}[t]) |
    #{fst flowOverall} =< theFlow and theFlow =< #{snd flowOverall}
  \##{activated} >= #{atLeastActive}
  theActivated#{upperFirst which}Transitions[#{activated}]
  #{connected (prepend "graphIsConnected") isConnected}
  #{isolated (prepend "noIsolatedNodes") isConnected}|]
  where
    (given, prepend, which)
      | underDefault = (("given" ++), (which ++) . upperFirst, "default")
      | otherwise    = (id, id, "")
    flow = prepend "flow"
    nodes = given "Nodes"
    places = given "Places"
    tokens = prepend "tokens"

connected :: String -> Maybe Bool -> String
connected p = maybe "" $ \c -> (if c then "" else "not ") ++ p

isolated :: String -> Maybe Bool -> String
isolated p = maybe p $ \c -> if c then "" else p

compAdvConstraints :: AdvConfig -> String
compAdvConstraints AdvConfig
                        { presenceOfSelfLoops, presenceOfSinkTransitions
                        , presenceOfSourceTransitions
                        } = [i|
  #{maybe "" petriLoops presenceOfSelfLoops}
  #{maybe "" petriSink presenceOfSinkTransitions}
  #{maybe "" petriSource presenceOfSourceTransitions}
|]
  where
    petriLoops = \case
      True  -> "some n : Nodes | selfLoop[n]"
      False -> "no n : Nodes | selfLoop[n]"
    petriSink = \case
      True  -> "some t : Transitions | sinkTransitions[t]"
      False -> "no t : Transitions | sinkTransitions[t]"
    petriSource = \case
      True  -> "some t : Transitions | sourceTransitions[t]"
      False -> "no t : Transitions | sourceTransitions[t]"

compChange :: ChangeConfig -> String
compChange ChangeConfig
                  {flowChangeOverall, maxFlowChangePerEdge
                  , tokenChangeOverall, maxTokenChangePerPlace
                  } = [i|
  (sum f, t : Nodes | abs[f.flowChange[t]]) = #{flowChangeOverall}
  maxFlowChangePerEdge[#{maxFlowChangePerEdge}]
  (sum p : Places | abs[p.tokenChange]) = #{tokenChangeOverall}
  maxTokenChangePerPlace[#{maxTokenChangePerPlace}]
|]

{-|
Generates signatures of the given kind, number of places and transitions.
-}
signatures
  :: String
  -- ^ What kind of signatures to generate
  -- (e.g., @"given"@ for @givenPlaces@ and @givenTransitions@)
  -> Int
  -- ^ How many places of that kind
  -> Int
  -- ^ How many transitions of that kind
  -> String
signatures what places transitions = intercalate "\n"
  $  [ [i|one sig P#{x} extends #{what}Places {}|]
     | x <- [1 .. places]]
  ++ [ [i|one sig T#{x} extends #{what}Transitions {}|]
     | x <- [1 .. transitions]]

taskInstance
  :: (MonadThrow m, RandomGen g, MonadAlloy m)
  => (f
    -> AlloyInstance
    -> RandT g m a)
  -> (config -> String)
  -> f
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> RandT g m a
taskInstance taskF alloyF parseF alloyC config segment = do
  let is = T.maxInstances (alloyC config)
  list <- getInstances is (T.timeout $ alloyC config) (alloyF config)
  when (null $ drop segment list)
    $ throwM NoInstanceAvailable
  inst <- case fromIntegral <$> is of
    Nothing -> randomInstance list
    Just n -> do
      x <- randomInSegment segment n
      case drop x list of
        x':_ -> return x'
        []   -> randomInstance list
  taskF parseF inst
  where
    randomInstance list = do
      n <- randomInSegment segment (1 + ((length list - segment - 1) `div` 4))
      return $ list !! n

randomInSegment :: (RandomGen g, Monad m) => Int -> Int -> RandT g m Int
randomInSegment segment segLength = do
  x <- liftRandT $ return . randomR ((0,) $ pred segLength)
  return $ segment + 4 * x

unscopedSingleSig
  :: MonadThrow m
  => AlloyInstance
  -> String
  -> String
  -> m (Set Object)
unscopedSingleSig inst st nd = do
  sig <- lookupSig (unscoped st) inst
  getSingleAs nd (return .: Object) sig

skolemVariable :: String -> String -> String
skolemVariable x y = '$' : x ++ '_' : y
