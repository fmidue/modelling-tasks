{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Output where

import Util
import Types (AssociationType(..), Connection(..), Syntax)
import Edges

import Control.Lens                     ((&), (.~), (^.))
import Data.Colour.Names                (black, white)
import Data.Graph.Inductive             (Gr)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
  (Label, Attribute (..), DPoint (..))
import Data.List
import Data.List.Split
import Data.Maybe                       (fromJust, fromMaybe, mapMaybe)
import Diagrams.Align                   (center)
import Diagrams.Angle                   (Angle, (@@), cosA, deg, sinA)
import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Combinators             (frame)
import Diagrams.Core                    (local, maxTraceP, moveTo)
import Diagrams.Core.Types              (Diagram, location, lookupName)
import Diagrams.Direction               (dirBetween)
import Diagrams.Names                   (named, toName)
import Diagrams.Points                  ((*.))
import Diagrams.TwoD                    hiding (arrow)
import Diagrams.TwoD.GraphViz           (drawGraph, layoutGraph', mkGraph)
import Diagrams.Util                    ((#), with)
import Graphics.SVGFonts
  (Spacing (..), TextOpts (..), Mode (..), lin, textSVG_)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Linear.Affine                    (Point (..), unP)
import System.FilePath                  (dropExtension)
import System.Random.Shuffle            (shuffleM)

connectionArrow :: Bool -> Maybe Attribute -> Connection -> [Attribute]
connectionArrow _          _   Inheritance =
  [arrowTo emptyArr]
connectionArrow printNames marking (Assoc Composition name from to isMarked) =
  arrow Composition ++ [HeadLabel (mult to)]
  ++ concat [maybe [] (:[]) marking | isMarked] ++ [toLabel name | printNames]
  ++ case from of
       (1, Just 1) -> []
       (0, Just 1) -> [TailLabel (mult from)]
       _           -> error $ "invalid composition multiplicity"
connectionArrow printNames marking (Assoc a name from to isMarked) =
  arrow a ++ [TailLabel (mult from), HeadLabel (mult to)]
  ++ concat [maybe [] (:[]) marking | isMarked] ++ [toLabel name | printNames]

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

drawCdFromSyntax :: Bool -> Maybe Attribute -> Syntax -> FilePath -> GraphvizOutput -> IO ()
drawCdFromSyntax printNames marking syntax file format = do
  let (classes, associations) = syntax
  let classNames = map fst classes
  let theNodes = classNames
  let inhEdges = mapMaybe (\(from,mto) -> fmap (\to -> (from, to, Inheritance)) mto) classes
  let classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
        where
          subs seen name
            | name `elem` seen = []
            | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((== Just name) . snd) classes)
  let assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  let assocEdges = map (\(a,n,m1,from,to,m2) -> (from, to, Assoc a n m1 m2 (shouldBeMarked from to classesWithSubclasses assocsBothWays))) associations
  let graph = mkGraph theNodes (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams {
                   fmtNode = \(_,l) -> [toLabel l,
                                        shape BoxShape, Margin $ DVal $ 0.04, Width 0, Height 0, FontSize 11],
                   fmtEdge = \(_,_,l) -> FontSize 11 : connectionArrow printNames marking l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

drawOdFromInstance :: Bool -> String -> FilePath -> GraphvizOutput -> IO ()
drawOdFromInstance printNames input file format = do
  let printArrows = False
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
  let theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
  let theEdges = map ((\[from,v,to] -> (from, to, takeWhile (/= '$') v)) . splitOn "->") $
                 filter (not . null) (splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine)))))
  let numberedNodes = zip [0..] theNodes
  let graph = mkGraph theNodes theEdges :: Gr String String
  objectNames <-
    map (\(i, l) -> (i, let [n,z] = splitOn "$" l in firstLower n ++ (if z == "0" then "" else z) ++ " "))
    <$> drop (length theNodes `div` 3)
    <$> shuffleM numberedNodes
  let objectNames' = (\(i, n) -> (fromMaybe "" $ lookup i numberedNodes, n)) <$> objectNames
  let params = nonClusteredParams {
                   fmtNode = \(i,l) -> [underlinedLabel (fromMaybe "" (lookup i objectNames) ++ ": " ++ takeWhile (/= '$') l),
                                         shape BoxShape, Margin $ DVal $ 0.04, Width 0, Height 0, FontSize 12],
                   fmtEdge = \(_,_,l) -> [edgeEnds NoDir, FontSize 12] ++ [toLabel l | printNames] }
  let undirected = Neato
  graph' <- layoutGraph'  params undirected graph
  sfont <- lin
  let qdiagram = drawGraph (renderNode sfont objectNames') (\_ _ _ _ _ _ -> mempty) graph'
      qdiagram' = drawGraph (\_ _ -> mempty) drawEdge graph'
      edgeAngle = 15 @@ deg
      arrowOpts
        | printArrows = with & arrowHead .~ varrow & headLength .~ local 6
        | otherwise   = with & arrowHead .~ noHead
      drawEdge fl fp tl tp l _ =
        (if printNames
         then (edgeLabel <>)
         else id)
        (if bothDirs
         then arrowBetween' (arrowOpts & arrowShaft .~ connectArc) arcs arce
         else qdiagram # connectOutside' arrowOpts fl tl)
        where
          edgeLabel = moveTo lp (center $ textSVG_ (TextOpts sfont INSIDE_H KERN False 14 14) l)
            # fc black
            # lc black
          bothDirs = containsEdge (tl, fl) theEdges
          ashift = 15
          a1 = angleBetween' fp tp ^. deg - ashift @@ deg
          a2 = angleBetween' tp fp ^. deg + ashift @@ deg
          connectArc = arc (dirBetween fp tp) edgeAngle
          vec = arce - arcs
          vecLen = sqrt $ ((vec ^. _x) ** 2) + ((vec ^. _y) ** 2)
          -- calculate start and endpoint of the connector as connectPerim' does
          Just sub1 = lookupName (toName fl) qdiagram'
          Just sub2 = lookupName (toName tl) qdiagram'
          os = location sub1
          oe = location sub2
          arcs = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
          arce = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
          -- calculate the label distance by using trigonometric triangle laws
          lp = arcs + (vec / 2) - if bothDirs then ld else 0
          rLen = vecLen / sinA edgeAngle
          sideHalfLen = sqrt $ rLen ** 2 - 1 / 4 * vecLen ** 2
          ld = ((rLen - sideHalfLen) / vecLen) *. rotate (90 @@ deg) vec
  let dotGraph = graphToDot params graph
  let file' = file ++ ".svg"
  renderSVG file' (mkWidth 250) qdiagram'
  putStrLn $ "Output written to " ++ file'
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphvizCommand undirected dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output
  where
    angleBetween' v1 v2 = signedAngleBetweenDirs (dirBetween v1 v2) xDir
    containsEdge (x, y) xs = not $ null [x | (x', y', _) <- xs, x == x', y == y']
    renderNode
     :: PreparedFont Double
     -> [(String, String)]
     -> String
     -> Point V2 Double
     -> Diagram B
    renderNode sfont objectNames t (P p) = translate p $ center $
      frame 0.4 (frame 1 (textSVG_ (TextOpts sfont INSIDE_H KERN True 16 16) (fromMaybe "" (lookup t objectNames) ++ ": " ++ takeWhile (/= '$') t) # snugCenterXY)
                    # fc black # lc black # bg white)
      # bg black
      # named t

varrow :: ArrowHT Double
varrow = arrowheadV (160 @@ deg)

arrowheadV :: RealFloat n => Angle n -> ArrowHT n
arrowheadV theta len shaftWidth = (jt, mempty)
  where
    shift right = translate (unP $ (factor * sinA theta * len / 2) *. unitY)
                . translate (unP $ (cosA theta * len / 2) *. unitX)
      where factor = if right then -1 else 1
    mtheta = - theta ^. deg @@ deg
    jt = shift True (rotate mtheta line) <> shift False (rotate theta line) <> translate (unP $ (shaftWidth * sinA theta / 2) *. unitX) tip
    tip = rotate (-90 @@ deg) (scaleY (sinA theta) (triangle shaftWidth))
    line = rect len shaftWidth
