module Output where

import Util
import Types (AssociationType(..), Connection(..), Syntax)
import Edges

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import System.FilePath (dropExtension)

connectionArrow :: Connection -> [Attribute]
connectionArrow Inheritance = [arrowTo emptyArr]
connectionArrow (Assoc Composition from to isRed) =
  case from of
    (1, Just 1) -> arrow Composition ++ [HeadLabel (mult to)] ++ [redColor | isRed]
    (0, Just 1) -> arrow Composition ++ [TailLabel (mult from), HeadLabel (mult to)] ++ [redColor | isRed]
connectionArrow (Assoc a from to isRed) = arrow a ++ [TailLabel (mult from), HeadLabel (mult to)] ++ [redColor | isRed]

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

drawCdFromSyntax :: Syntax -> FilePath -> GraphvizOutput -> IO ()
drawCdFromSyntax syntax file format = do
  let (classes, associations) = syntax
  let classNames = map fst classes
  let theNodes = classNames
  let inhEdges = mapMaybe (\(from,mto) -> fmap (\to -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Inheritance)) mto) classes
  let classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
        where
          subs seen name
            | name `elem` seen = []
            | otherwise = name : concatMap (subs (name:seen) . fst) (filter ((== Just name) . snd) classes)
  let assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  let assocEdges = map (\(a,_,m1,from,to,m2) -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), Assoc a m1 m2 (shouldBeRed from to classesWithSubclasses assocsBothWays))) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges) :: Gr String Connection
  let dotGraph = graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [toLabel l, shape BoxShape], fmtEdge = \(_,_,l) -> connectionArrow l }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output

drawOdFromInstance :: String -> FilePath -> GraphvizOutput -> IO ()
drawOdFromInstance input file format = do
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
  let theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
  let theEdges = map ((\[from,_,to] -> (fromJust (elemIndex from theNodes), fromJust (elemIndex to theNodes), ())) . splitOn "->") $
                 filter (not . null) (splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj<:get=" objGetLine)))))
  let graph = undir (mkGraph (zip [0..] theNodes) theEdges) :: Gr String ()
  let dotGraph = setDirectedness graphToDot (nonClusteredParams { fmtNode = \(_,l) -> [underlinedLabel (firstLower l ++ " : " ++ takeWhile (/= '$') l), shape BoxShape] }) graph
  quitWithoutGraphviz "Please install GraphViz executables from http://graphviz.org/ and put them on your PATH"
  output <- addExtension (runGraphviz dotGraph) format (dropExtension file)
  putStrLn $ "Output written to " ++ output
