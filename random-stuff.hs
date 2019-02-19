module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (getAllMutationResults)
import Output
import Transform (transform)
import Types

import Control.Monad       (unless)
import Data.List
import Data.List.Split     (splitOn)
import Data.GraphViz
import Data.Time.LocalTime

import System.FilePath (searchPathSeparator)
import System.IO
import System.Process
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  let config = Config {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Nothing),
          associations = (Nothing, Nothing),
          compositions = (Nothing, Nothing),
          inheritances = (Nothing, Nothing),
          searchSpace  = 10,
          output       = "output",
          maxInstances = -1
        }
  (names, edges) <- generate config
  let syntax = fromEdges names edges
  drawCdFromSyntax syntax (output config ++ "1") Pdf
  unless (anyRedEdge syntax) $ do
    time <- getZonedTime
    let (part1, part2, part3, part4, part5) = transform syntax "" (show time)
        als = part1 ++ part2 ++ part3 ++ part4 ++ part5
    instances <- getAlloyInstances (maxInstances config) als
    mutations <- shuffleM $ getAllMutationResults names edges
    let cd2 = fromEdges names $ getFirstValid names mutations
    drawCdFromSyntax cd2 (output config ++ "2") Pdf
    mapM_ (\(i, insta) -> drawOdFromInstance insta (show i) Pdf) (zip [1 :: Integer ..] instances)

getFirstValid :: [String] -> [[DiagramEdge]] -> [DiagramEdge]
getFirstValid _     []
  = error "There is no (further) valid mutation for this chart!"
getFirstValid names (x:xs)
  | checkMultiEdge x, not (anyRedEdge $ fromEdges names x)
  = x
  | otherwise
  = getFirstValid names xs

getAlloyInstances :: Int -> String -> IO [String]
getAlloyInstances maxInsta content = do
  let callAlloy = proc "java" ["-cp", '.' : searchPathSeparator :  "alloy/Alloy-5.0.0.1.jar",
                               "alloy.RunAlloy", show maxInsta]
  (Just hin, Just hout, _, _) <- createProcess callAlloy { std_out = CreatePipe, std_in = CreatePipe }
  hPutStr hin content
  hClose hin
  fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  where
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> hGetLine h <*> getWholeOutput h
