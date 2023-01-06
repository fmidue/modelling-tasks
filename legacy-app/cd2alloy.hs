module Main (main) where

import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.CD2Alloy.Transform (Parts (..), createRunCommand, transform)
import Modelling.CdOd.Types (maxFiveObjects)

import Control.Monad
import Data.Time.LocalTime
import System.Environment (getArgs)

run :: String -> Maybe FilePath -> Bool -> String -> IO ()
run input output template index = do
  let tokens = lexer input
  let syntax = parser tokens
  time <- getZonedTime
  let parts = transform syntax [] maxFiveObjects Nothing False index (show time)
      p1 = part1 parts
      p2 = part2 parts
      p3 = part3 parts
      p4 = part4 parts
      p5 = createRunCommand
        ("cd" ++ index)
        (length $ fst syntax)
        maxFiveObjects
        parts
  case output of
    Just file -> do
      when template $ let out = file ++ ".part1" in writeFile out p1 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part2" in writeFile out p2 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part3" in writeFile out p3 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part4" in writeFile out p4 >> putStrLn ("Some output written to " ++ out)
      when template $ let out = file ++ ".part5" in writeFile out p5 >> putStrLn ("Some output written to " ++ out)
    Nothing -> putStrLn $ (if template then p1 else "") ++ p2 ++ p3 ++ p4 ++ (if template then p5 else "")

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run contents Nothing False ""
   [input] -> readFile input >>= \contents -> run contents Nothing False ""
   [input, output] -> readFile input >>= \contents -> run contents (Just output) True ""
   [input, output, index] -> readFile input >>= \contents -> run contents (Just output) True (show (read index :: Int))
   _ -> error "zu viele Parameter"
