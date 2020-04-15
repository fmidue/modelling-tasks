{-# Language QuasiQuotes #-}
module PetriParser where

import Data.List
import Data.Maybe
import Data.String.Interpolate
import Language.Alloy.Call
import qualified Data.Set as Set
import Interactions
import AuxFunctions
      
type TripSet = Set.Set (Object,Object,Object)


convertPetri :: AlloyInstance -> IO()
convertPetri inst = do
  filterFlow inst 
  startMark inst
      
                          --Transitionen--

--[([(3)],[(3)])] List of TransSets
filterFlow :: AlloyInstance -> IO()
filterFlow inst = do
  let trn = singleSig inst (Set.fromAscList ["this","Transitions",""])
  case trn of
    Left error -> print error
    Right trans -> do
      let out = tripleSig inst 
      case out of 
        Left error -> print error
        Right set -> do
          let flow = filterTrans (Set.toList trans) set
          let plcs = singleSig inst (Set.fromAscList ["this","Places",""])
          case plcs of 
            Left error -> print error
            Right places -> do
            print $ convertToTrans (Set.toList places) flow
            
      

filterTrans :: [Object] -> TripSet -> [(TripSet,TripSet)]
filterTrans [] _ = []
filterTrans (t:rs) set = ((filterSndTrip t set,filterFstTrip t set) : (filterTrans rs set))

convertToTrans :: [Object] -> [(TripSet,TripSet)] -> [([Int],[Int])]
convertToTrans _ [] = []
convertToTrans ls ((a,b):rs) = ((helpConvertPre ls (Set.toList a),helpConvertPost ls (Set.toList b)) 
                                : convertToTrans ls rs )



                         --Startmarkierung--
startMark :: AlloyInstance -> IO ()
startMark inst = do
  let mark = doubleSig inst (Set.fromAscList ["this","Places","tokens"])
  case mark of
    Left error -> print error
    Right smark -> do 
      let mList =  convertTuple (Set.toList smark)
      print mList
      
convertTuple :: [(Object,Object)] -> [Int]
convertTuple [] = []
convertTuple ((_,i):rs) = ((read (objectName i) :: Int) : (convertTuple rs))

  

                            --Hilfsfunktionen--
-- Instance -> scoped? -> relations (e.g. ["this","Nodes","flow"])
singleSig :: AlloyInstance -> Set.Set String -> Either String (Set.Set Object)
singleSig inst set = do
  sig <- lookupSig (scoped (Set.elemAt 0 set) (Set.elemAt 1 set)) inst
  getSingle (Set.elemAt 2 set) sig
                            
doubleSig :: AlloyInstance -> Set.Set String -> Either String (Set.Set (Object,Object))
doubleSig inst set = do
  sig <- lookupSig (scoped (Set.elemAt 0 set) (Set.elemAt 1 set)) inst
  getDouble (Set.elemAt 2 set) sig

tripleSig :: AlloyInstance -> Either String (Set.Set (Object,Object,Object))
tripleSig inst = do
  sig <- lookupSig (scoped "this" "Nodes") inst
  getTriple "flow" sig
  
--Filter for Stuff--
filterFstTrip :: Object -> (Set.Set (Object,Object,Object)) -> (Set.Set (Object,Object,Object))
filterFstTrip a set = Set.filter (\s -> helpFilter a s) set
  where helpFilter a (x,_,_) = a == x
  
filterSndTrip :: Object -> (Set.Set (Object,Object,Object)) -> (Set.Set (Object,Object,Object))
filterSndTrip a set = Set.filter (\s -> helpFilter a s) set
  where helpFilter a (_,x,_) = a == x
  
helpConvertPre :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPre [] _ = []
helpConvertPre (p:rp) [] = (0: helpConvertPre rp [])
helpConvertPre (p:rp) ((a,b,x):rt)
 | p == a = ((read (objectName x) :: Int) : helpConvertPre rp rt)
 | otherwise = (0 : helpConvertPre rp ((a,b,x):rt) )
 
helpConvertPost :: [Object] -> [(Object,Object,Object)] -> [Int]
helpConvertPost [] _ = []
helpConvertPost (p:rp) [] = (0: helpConvertPost rp [])
helpConvertPost (p:rp) ((a,b,x):rt)
 | p == b = ((read (objectName x) :: Int) : helpConvertPost rp rt)
 | otherwise = (0 : helpConvertPost rp ((a,b,x):rt) )
----------------------------------------Testing--------------------------------------------------

--Startmarkierung--
testPParser :: IO()
testPParser = do
  list <- getInstances (Just 5) petriNetA
  convertPetri (head list)

testMark :: IO ()
testMark = do
  list <- getInstances (Just 5) petriNetA
  mark <- startMark (head list)
  --doubleSig (head list) (Set.fromAscList ["this","Places","tokens"])
  print mark
--Flow--
testFlow :: IO()
testFlow = do
  list <- getInstances (Just 5) petriNetA
  filterFlow (head list)

--Stuff--
getI :: String -> IO [AlloyInstance]
getI inp = getInstances (Just 5) inp

testInput :: IO[AlloyInstance]
testInput = do
  pref <- getInput
  let inp = petriAlloy pref
  getI inp

testIt :: IO ()
testIt = do
  list <- getInstances (Just 5) petriNetA
  --convert Object to a String -> für single -> (fmap objectName.Set.toList) <$>  --
  let out = (tripleSig (head list))
  let elems = (singleSig (head list) (Set.fromAscList ["this","Nodes",""]))
  case out of
    Left error -> print $ "ERROR: "++ error
    Right set -> do 
      case elems of
        Left error ->  print $ "ERROR: "++ error
        Right elms -> do
          let a = getFirstElem elms
          let triple = Set.elemAt 0 (filterFstTrip a set)
          testSingle triple
          print triple
          
testSingle :: (Object,Object,Object) -> IO ()
testSingle (a,b,c) = do 
  print $ objectName a

----------------------------------------------------------------------
----------------------------------------------------------------------
petriNetA :: String
petriNetA = [i|module scenarios/examples/PetriNetA 
#{modulePetriSignature}
//default Petri net

one sig S1 extends givenPlaces{}
one sig S2 extends givenPlaces{}
one sig S3 extends givenPlaces{}
one sig T1 extends givenTransitions{}
one sig T2 extends givenTransitions{}
one sig T3 extends givenTransitions{}

fact {
  S1.defaultTokens = 1
  S2.defaultTokens = 1
  S3.defaultTokens = 0

  S1.defaultFlow[T1] = 1
  S1.defaultFlow[T2] = 1
  S1.defaultFlow[T3] = 1

  S2.defaultFlow[T2] = 1
  no S2.defaultFlow[Transitions - T2]

  S3.defaultFlow[T2] = 1
  no S3.defaultFlow[Transitions - T2]

  T1.defaultFlow[S2] = 1
  no T1.defaultFlow[Places - S2]

  no T2.defaultFlow[Places]

  T3.defaultFlow[S3] = 1
  no T3.defaultFlow[Places - S3]

}
|]