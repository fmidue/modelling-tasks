{-# Language QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module PetriAlloy where 

import Data.String.Interpolate
import Data.FileEmbed
import Types

petriScope :: Input -> Int
petriScope Input{places,transitions} =
  (places+transitions)*2
  
petriLoops :: Bool -> String
petriLoops = \case
 True  -> "some p : Places, t : Transitions | selfLoop[p, t]"
 False -> "no p : Places, t : Transitions | selfLoop[p, t]"

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

--Bigger Net needs bigger "run for x"
-- make flowSum dynamic with input

petriNetConstraints :: Input -> String 
petriNetConstraints Input{atLeastActiv,minTknsOverall,maxTknsOverall,maxTknsPerPlace,
                        minFlowOverall,maxFlowOverall,maxFlowPerEdge,
                        presenceSelfLoops,presenceSinkTrans,presenceSourceTrans} = [i|let t = tokenSum[Places] | t >= #{minTknsOverall} and t <= #{maxTknsOverall}
  all p : Places | p.tokens =< #{maxTknsPerPlace}
  all weight : Nodes.flow[Nodes] | weight =< #{maxFlowPerEdge}
  let flow = flowSum[Nodes,Nodes] | flow >= #{minFlowOverall} and #{maxFlowOverall} >= flow
  #ts >= #{atLeastActiv}
  theActivatedTransitions[ts]
  noIsolatedNodes[]
  graphIsConnected[]
  #{maybe "" petriLoops presenceSelfLoops}
  #{maybe "" petriSink presenceSinkTrans}
  #{maybe "" petriSource presenceSourceTrans}
|]

petriNetRnd :: Input -> String
petriNetRnd input@Input{places,transitions} = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets [ts : set Transitions] {
  #Places = #{places}
  #Transitions = #{transitions}
  #{petriNetConstraints input}
  
}
run showNets for #{petriScope input}

|]