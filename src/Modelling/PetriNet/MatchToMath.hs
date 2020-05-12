{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.MatchToMath where

import Modelling.PetriNet.Alloy          (petriNetRnd, renderFalse)
import Modelling.PetriNet.Diagram
import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Parser
import Modelling.PetriNet.Types

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call               (AlloyInstance,getInstances)
import Text.LaTeX                        (LaTeX)

--True Task1 <-> False Task1a
matchToMath :: Bool -> PetriTask1Config -> IO (Diagram B, LaTeX, Either [(Diagram B, Change)] [(LaTeX, Change)])
matchToMath switch inp@PetriTask1Config{basicTask1} = do
  list <- getInstances (Just 1) (petriNetRnd basicTask1)
  let out = convertPetri "tokens" (head list)
  case out of
    Left merror -> error merror
    Right petri -> do
      rightNet <- drawNet petri (graphLayout basicTask1)
      let tex
           | switch    = uebung petri 1 switch
           | otherwise = uebung petri 2 switch
      let f = renderFalse petri inp
      fList <- getInstances (Just 3) f
      let (fNets,changes) = falseList fList []
      if switch then do
        fDia <- mapM (flip drawNet (graphLayout basicTask1)) fNets
        return (rightNet, tex, Left $ zip fDia changes)
      else do
        let fTex = map createPetriTex fNets
        return (rightNet, tex, Right $ zip fTex changes)

falseList :: [AlloyInstance] -> [Petri] -> ([Petri],[Change])
falseList [] _       = ([],[])
falseList (inst:rs) usedP = do
  let fParsed = runFalseParser inst
  case fParsed of
    (Left ferror,Left cError) -> error $ ferror ++ cError
    (Left ferror, _)          -> error ferror
    (_,Left cError)           -> error cError
    (Right fNet,Right change) -> do
      let rest@(rf,rc) = falseList rs (fNet:usedP)
      if fNet `elem` usedP 
      then rest
      else (fNet:rf,change:rc)
      
runFalseParser :: AlloyInstance -> (Either String Petri,Either String Change)
runFalseParser alloy = do
  let petri = convertPetri "tokens" alloy
  let change = parseChange alloy
  (petri,change)
  
checkTask1Config :: PetriTask1Config -> Maybe String
checkTask1Config PetriTask1Config{basicTask1 = PetriBasicConfig{places,transitions
                   , minTokensOverall,maxTokensOverall,maxTokensPerPlace
                   , minFlowOverall,maxFlowOverall,maxFlowPerEdge}
                 , tokenChangeOverall, flowChangeOverall
                 , maxFlowChangePerEdge, maxTokenChangePerPlace}
 | tokenChangeOverall < 0
  = Just "The parameter 'tokenChangeOverall' must be non-negative."
 | maxTokenChangePerPlace < 0
  = Just "The parameter 'maxTokenChangePerPlace' must be non-negative."
 | maxTokenChangePerPlace > tokenChangeOverall
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'tokenChangeOverall'."
 | maxTokenChangePerPlace > maxTokensPerPlace
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'maxTokensPerPlace'."
 | tokenChangeOverall > maxTokensOverall - minTokensOverall
  = Just "With 'tokenChangeOverall', stay within the range of tokens overall."
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
 | flowChangeOverall > maxFlowOverall - minFlowOverall
  = Just "With 'flowChangeOverall', stay within the range of flow overall."
 | 2 * places * transitions * maxFlowChangePerEdge < flowChangeOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonably high, given the other parameters."
 | otherwise
  = Nothing