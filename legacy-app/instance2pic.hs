module Main (main) where
import qualified Data.ByteString.Char8            as BS (pack, writeFile)

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.Output            (drawOd)
import Modelling.CdOd.Types             (anonymiseObjects)

import Control.Monad (void)
import Control.Monad.Random             (evalRandT, mkStdGen)
import Data.Char                        (toUpper)
import Data.GraphViz                    (DirType (NoDir))
import Data.Ratio                       ((%))

import System.Environment (getArgs)
import Language.Alloy.Debug             (parseInstance)

main :: IO ()
main = do
  args <- getArgs
  void $ case args of
   [] -> error "possible links required (first parameter)"
   [xs] -> getContents >>= drawOdToFile (read xs) "output"
   [xs, file] -> readFile file >>= drawOdToFile (read xs) file
   [xs, file, format]
     | map toUpper format == "SVG" -> readFile file >>= drawOdToFile (read xs) file
     | otherwise -> error $ "format " ++ format
         ++ "is not supported, only SVG is supported"
   _ -> error "zu viele Parameter"

drawOdToFile :: [String] -> FilePath -> String -> IO ()
drawOdToFile possibleLinks file contents = do
  i <- parseInstance (BS.pack contents)
  od <- alloyInstanceToOd Nothing possibleLinks i
  od' <- flip evalRandT (mkStdGen 0) $ anonymiseObjects (1 % 3) od
  renderedOd <- drawOd od' Nothing NoDir False
  let filename = file ++ ".svg"
  BS.writeFile filename renderedOd
  putStrLn $ "Output written to " ++ filename
