module Main (main) where

import Types
import Generate
import Edges
import Transform (transform)
import Output

import Control.Monad
import Data.Time.LocalTime

import Data.GraphViz

main :: IO ()
main = do
 syntax <-
  generate Config {
      classes      = (Just 4, Just 4),
      aggregations = (Nothing, Nothing),
      associations = (Nothing, Nothing),
      compositions = (Nothing, Nothing),
      inheritances = (Nothing, Nothing),
      searchSpace  = 10
    }
 let output = "output"
 drawCdFromSyntax syntax output Pdf
 unless (anyRedEdge syntax) $
  do
    time <- getZonedTime
    let (part1, part2, part3, part4, part5) = transform syntax "" (show time)
    let out = output ++ ".als"
    writeFile out (part1 ++ part2 ++ part3 ++ part4 ++ part5)
    putStrLn ("More output written to " ++ out)
    instances <- giveMeInstances
    mapM_ (\(i, insta) -> drawOdFromInstance insta (show i) Pdf) (zip [1 :: Integer ..] instances)

giveMeInstances :: IO [String]
giveMeInstances = return []
