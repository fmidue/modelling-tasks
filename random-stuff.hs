module Main (main) where

import Edges
import Generate (generate)
import Output
import Transform (transform)
import Types

import Control.Monad       (unless)
import Data.List
import Data.List.Split     (splitOn)
import Data.GraphViz
import Data.Time.LocalTime

import System.IO
import System.Process

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
  syntax <- generate config
  drawCdFromSyntax syntax (output config) Pdf
  unless (anyRedEdge syntax) $ do
    time <- getZonedTime
    let (part1, part2, part3, part4, part5) = transform syntax "" (show time)
        als = part1 ++ part2 ++ part3 ++ part4 ++ part5
    instances <- giveMeInstances config als
    mapM_ (\(i, insta) -> drawOdFromInstance insta (show i) Pdf) (zip [1 :: Integer ..] instances)

giveMeInstances :: Config -> String -> IO [String]
giveMeInstances c content = do
  let callAlloy = proc "java" ["-cp", "Alloy-5.0.0.1.jar", "RunAlloy.java", show $ maxInstances c]
  (Just hin, Just hout, _, _) <- createProcess callAlloy { std_out = CreatePipe, std_in = CreatePipe }
  hPutStr hin content
  fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  where
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> hGetLine h <*> getWholeOutput h
