{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.Diagram (drawNet) where

import Modelling.PetriNet.Types

import qualified Diagrams.TwoD.GraphViz           as GV
import qualified Data.Map                         as M (foldlWithKey)

import Diagrams.Backend.SVG             (B)
import Diagrams.Path                    (pathPoints)
import Diagrams.Prelude
import Data.Graph.Inductive.Graph       (mkGraph)
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.GraphViz                    hiding (Path)
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
  

----------------------Preparing a PetriNet for Graph--------------------
prepNet :: Petri -> Gr (String, Maybe Int) String
prepNet Petri{initialMarking,trans} =
  mkGraph (prepPlaces (length initialMarking) 0 initialMarking
           ++ prepTrans nA (length initialMarking) 1)
  (prepEdges (length initialMarking) trans)
  where nA = length initialMarking + length trans
 
--AnzahlStellen -> startIndex -> StartMarkierung
prepPlaces :: Int -> Int -> [Int] -> [(Int,(String,Maybe Int))]
prepPlaces _ _ []     = []
prepPlaces s i (m:rm) = (i,("s" ++ show (i+1), Just m)):prepPlaces s (i+1) rm 

--GesamtAnzahl(Stellen+Trans) -> startIndex
prepTrans :: Int -> Int -> Int -> [(Int,(String,Maybe Int))]
prepTrans s i t
 | s > i         = (i,("t" ++ show t,Nothing)):prepTrans s (i+1) (t+1)
 | otherwise     = []
 
{-
  ex: extern Index of Transitions
  i : intern Index of Places
-}

--Counter-> transitions -> Ausgabe
prepEdges :: Int -> [Transition] -> [(Int,Int,String)]
prepEdges _ [] = []
prepEdges ex ((p,post):rt) = createPre ex 0 p
                            ++ createPost ex 0 post
                            ++ prepEdges (ex+1) rt

--ExternCounter -> InternCounter->List->Ausgabe
createPre :: Int -> Int -> Marking -> [(Int,Int,String)]
createPre _ _ [] = []
createPre ex i (m:rm) 
 | m /= 0    = (i,ex,show m):createPre ex (i+1) rm
 | otherwise = createPre ex (i+1) rm

createPost :: Int -> Int -> Marking -> [(Int,Int,String)]
createPost _ _ [] = []
createPost ex i (m:rm) 
 | m /= 0    = (ex,i,show m):createPost ex (i+1) rm
 | otherwise = createPost ex (i+1) rm

-------------------------------------------------------------------------
drawNet :: Petri -> GraphvizCommand -> IO (Diagram B)
drawNet pnet gc = do
  let gnet = prepNet pnet
  graph <- GV.layoutGraph gc gnet
  pfont <- lin
  let (nodes, edges) = GV.getGraph graph
      gnodes = M.foldlWithKey (\g l p -> g `atop` drawNode pfont l p) mempty nodes
      gedges = foldl (\g (l1, l2, l, p) -> g # drawEdge pfont l l1 l2 p) gnodes edges
  return (gedges # frame 1)

drawNode :: PreparedFont Double -> (String,Maybe Int) -> Point V2 Double -> Diagram B
drawNode pfont (l, Nothing) p  = place
  (center (text' pfont l)
    `atop` rect 20 20 # named l)
  p
drawNode pfont (l,Just i) p  = place
  (center (text' pfont l)
    `atop` text' pfont (show i :: String) # translate (r2(-3,-15))
    `atop` circle 20 # named l)
  p

drawEdge :: PreparedFont Double -> String -> (String,Maybe Int) -> (String,Maybe Int) -> Path V2 Double -> Diagram B -> Diagram B
drawEdge f l (l1,_) (l2,_) path d = 
  let opts p = with & arrowShaft .~ (unLoc . head $ pathTrails p)
      points = concat $ pathPoints path
      labelPoint = points !! (length points `div` 2)
  in connectOutside' (opts path) l1 l2 d
     `atop` place (text' f l) labelPoint

text' :: PreparedFont Double -> String -> Diagram B
text' pfont t =
  textSVG_ (TextOpts pfont INSIDE_H KERN False 18 18) t
  # fc black
  # lc black

-----------------------------------------------------------------------
-- renderNet :: String -> Petri -> GraphvizCommand -> IO ()
-- renderNet name petri gc = do
  -- diagram <- drawNet petri gc
  -- renderSVG (name++".svg") (mkWidth 200) diagram
  -- print "PetriNetz erstellt"
