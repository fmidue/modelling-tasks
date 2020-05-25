{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Alloy 
  (petriNetRnd,renderFalse,petriNetFindConfl,petriNetPickConfl
  ,petriNetFindConcur,petriNetPickConcur,petriScope) 
  where

import Modelling.PetriNet.Types

import Data.String.Interpolate
import Data.FileEmbed

petriScope :: BasicConfig -> Int
petriScope BasicConfig{places,transitions,maxFlowPerEdge} = max
  (ceiling ( 2
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) places
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) transitions
  + ((logBase :: Double -> Double -> Double) 2.0 . fromIntegral) maxFlowPerEdge
  ))
  (places+transitions)
  
petriLoops :: Bool -> String
petriLoops = \case
 True  -> "some n : Nodes | selfLoop[n]"
 False -> "no n : Nodes | selfLoop[n]"

petriSink :: Bool -> String
petriSink = \case
 True  -> "some t : Transitions | sinkTransitions[t]"
 False -> "no t : Transitions | sinkTransitions[t]"

petriSource :: Bool -> String
petriSource = \case
 True  -> "some t : Transitions | sourceTransitions[t]"
 False -> "no t : Transitions | sourceTransitions[t]"

modulePetriSignature :: String
modulePetriSignature = removeLines 2 $(embedStringFile "lib/Alloy/PetriSignature.als")

modulePetriAdditions :: String
modulePetriAdditions = removeLines 11 $(embedStringFile "lib/Alloy/PetriAdditions.als")

moduleHelpers :: String
moduleHelpers = removeLines 4 $(embedStringFile "lib/Alloy/Helpers.als")

modulePetriConcepts :: String 
modulePetriConcepts = removeLines 5 $(embedStringFile "lib/Alloy/PetriConcepts.als")

modulePetriConstraints :: String
modulePetriConstraints = removeLines 4 $(embedStringFile "lib/Alloy/PetriConstraints.als")

removeLines :: Int -> String -> String
removeLines n = unlines . drop n . lines

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd input@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [activatedTrans : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints input}
  #{compAdvConstraints advConfig}
  
}
run showNets for #{petriScope input}

|]

renderFalse :: Petri -> MathConfig -> String
renderFalse Petri{initialMarking,trans}
    MathConfig{basicTask,advTask,changeTask} = [i| module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{givPlaces (length initialMarking)}
#{givTrans (length trans)}

fact{
  #{initialMark 1 initialMarking}

  #{defFlow 1 trans}
}

pred showFalseNets[activatedTrans : set Transitions]{
  #{compBasicConstraints basicTask}
  #{compAdvConstraints advTask}
  #{compChange changeTask}
  
}

run showFalseNets for #{petriScope basicTask}

|]

--Conflict--

specCompConfl :: BasicConfig -> ChangeConfig -> String
specCompConfl basic@BasicConfig{places,transitions} change = [i|
activatedTrans,conflictedTransitions: set Transitions, conflictPlace : Places] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints basic}
  #{compChange change}
  #{compConflict}
|]

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig{basicTask,advTask,changeTask} = [i|module PetriNetConfl

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred showRelNets [ #{specCompConfl basicTask changeTask}

  #{compAdvConstraints advTask}
  
}
run showRelNets for #{petriScope basicTask}

|]

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl p@PickConflictConfig{basicTask = BasicConfig{atLeastActive},changeTask} = [i|module PetriNetConfl

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred showRelNets [defaultActivTrans, #{specCompConfl (basicTask(p :: PickConflictConfig)) changeTask}

  #{compDefaultConstraints atLeastActive}
}
run showRelNets for #{petriScope (basicTask(p :: PickConflictConfig))}

|]

--Concurrency--
specCompConcur :: BasicConfig -> ChangeConfig -> String
specCompConcur basic@BasicConfig{places,transitions} change = [i|
activatedTrans,concurrentTransitions: set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{compBasicConstraints basic}
  #{compChange change}
  #{compConcurrency}
|]

petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{basicTask,advTask,changeTask} = [i|module PetriNetConcur

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred showRelNets [ #{specCompConcur basicTask changeTask}

  #{compAdvConstraints advTask}
  
}
run showRelNets for #{petriScope basicTask}

|]

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur p@PickConcurrencyConfig{basicTask = BasicConfig{atLeastActive},changeTask} = [i|module PetriNetConcur

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred showRelNets [defaultActivTrans, #{specCompConcur (basicTask(p :: PickConcurrencyConfig)) changeTask}

  #{compDefaultConstraints atLeastActive}
}
run showRelNets for #{petriScope (basicTask(p :: PickConcurrencyConfig))}

|]

----------------------"Building-Kit"----------------------------

-- Needs: activatedTrans : set Transitions
compBasicConstraints :: BasicConfig -> String
compBasicConstraints BasicConfig
                        {atLeastActive,minTokensOverall
                        ,maxTokensOverall,maxTokensPerPlace
                        , minFlowOverall,maxFlowOverall,maxFlowPerEdge
                        } = [i|
  let t = tokenSum[Places] | t >= #{minTokensOverall} and t <= #{maxTokensOverall}
  all p : Places | p.tokens =< #{maxTokensPerPlace}
  all weight : Nodes.flow[Nodes] | weight =< #{maxFlowPerEdge}
  let flow = flowSum[Nodes,Nodes] | flow >= #{minFlowOverall} and #{maxFlowOverall} >= flow
  #activatedTrans >= #{atLeastActive}
  theActivatedTransitions[activatedTrans]
  graphIsConnected[]
  
|]

compAdvConstraints :: AdvConfig -> String
compAdvConstraints AdvConfig
                        { presenceOfSelfLoops, presenceOfSinkTransitions
                        , presenceOfSourceTransitions
                        } = [i| 
  #{maybe "" petriLoops presenceOfSelfLoops}
  #{maybe "" petriSink presenceOfSinkTransitions}
  #{maybe "" petriSource presenceOfSourceTransitions}
|]

--Needs: defaultActivTrans : set Transitions
compDefaultConstraints :: Int -> String
compDefaultConstraints atLeastActive = [i|
  defaultGraphIsConnected[]
  #defaultActivTrans >= #{atLeastActive}
  theActivatedDefaultTransitions[defaultActivTrans]
|]

compChange :: ChangeConfig -> String
compChange ChangeConfig
                  {flowChangeOverall, maxFlowChangePerEdge
                  , tokenChangeOverall, maxTokenChangePerPlace
                  } = [i|
  flowChangeAbsolutesSum[Nodes,Nodes] = #{flowChangeOverall}
  maxFlowChangePerEdge [#{maxFlowChangePerEdge}]
  tokenChangeAbsolutesSum[Places] = #{tokenChangeOverall}
  maxTokenChangePerPlace [#{maxTokenChangePerPlace}]
|]

compConcurrency :: String
compConcurrency = [i|
  no x,y : Transitions | concurrentDefault[x+y] and x != y
  some concurTrans1, concurTrans2 : Transitions | concurrentTransitions = concurTrans1 + concurTrans2
  and concurTrans1 != concurTrans2
  and concurrent [concurTrans1+concurTrans2]
  and all u,v : Transitions | concurrent[u+v] and u != v implies concurTrans1 + concurTrans2 = u + v
|]

--Needs: conflictedTransitions: set Transitions, conflictTrans1,conflictTrans2 : Transitions, conflictPlace : Places
compConflict :: String
compConflict = [i|
  no x,y : Transitions, z : Places | conflictDefault[x,y,z]
  some conflictTrans1, conflictTrans2 : Transitions | conflictedTransitions = conflictTrans1 + conflictTrans2 
  and conflict [conflictTrans1, conflictTrans2, conflictPlace] and all u,v : Transitions, q : Places 
    | conflict[u,v,q] implies conflictTrans1 + conflictTrans2 = u + v
  
|]

givPlaces :: Int -> String
givPlaces 0 = ""
givPlaces p = "one sig S"++show p++" extends givenPlaces{} \n"++ givPlaces (p-1)

givTrans :: Int -> String
givTrans 0 = ""
givTrans t = "one sig T"++show t++" extends givenTransitions{} \n"++ givTrans (t-1)

initialMark ::Int -> Marking -> String
initialMark _ []      = ""
initialMark iM (m:rm) ="S"++ show iM ++".defaultTokens = "++ show m ++"\n  " ++ initialMark (iM+1) rm

defFlow :: Int -> [Transition] -> String
defFlow _ []            = ""
defFlow iT ((pr,po):rt) = flowPre iT 1 pr ++ flowPost iT 1 po ++ defFlow (iT+1) rt

flowPre :: Int -> Int -> [Int] -> String
flowPre _ _ [] = ""
flowPre iT iM (m:rm)
 | m == 0     = "no S"++ show iM ++".defaultFlow[T"++ show iT ++"]\n  " ++ flowPre iT (iM+1) rm
 | otherwise  = "S"++ show iM ++".defaultFlow[T"++ show iT ++"] = "++ show m ++"\n  "
                       ++ flowPre iT (iM+1) rm

flowPost :: Int -> Int -> [Int] -> String
flowPost _ _ [] = ""
flowPost iT iM (m:rm)
 | m == 0     = "no T"++ show iT ++".defaultFlow[S"++ show iM ++"]\n  " ++ flowPost iT (iM+1) rm
 | otherwise  = "T"++ show iT ++".defaultFlow[S"++ show iM ++"] = "++ show m ++"\n  "
                        ++ flowPost iT (iM+1) rm
