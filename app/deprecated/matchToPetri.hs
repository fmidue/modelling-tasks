module Main where

import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.MatchPetri(
  MatchPetriConfig(..),
  MatchPetriInstance (..),
  pickRandomLayout, matchPetriComponentsText, matchPetriTaskDescription, defaultMatchPetriConfig, matchPetriAlloy)
import Modelling.ActivityDiagram.Petrinet (PetriKey(label))
import Modelling.ActivityDiagram.PlantUMLConverter(convertToPlantUML)
import Language.Alloy.Call (getInstances)
import Language.PlantUML.Call (DiagramType(SVG), drawPlantUMLDiagram)

import Modelling.PetriNet.Diagram (cacheNet)
import Data.GraphViz.Commands (GraphvizCommand(Dot))
import Control.Monad.Except(runExceptT)
import Data.Tuple.Extra (fst3, snd3, thd3)


main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToFolder:xs' -> do
      let conf = defaultMatchPetriConfig
      inst <- getInstances (Just 50) $ matchPetriAlloy conf
      folders <- createExerciseFolders pathToFolder (length inst)
      let ad = map (failWith id . parseInstance) inst
          matchPetri = map (\x -> matchPetriComponentsText $ MatchPetriInstance{activityDiagram = x, seed=123, graphvizCmd=Dot}) ad
          plantumlstring = map (convertToPlantUML . fst3) matchPetri
          petri = map snd3 matchPetri
          taskDescription = replicate (length folders) matchPetriTaskDescription
          taskSolution = map thd3 matchPetri
      svg <- mapM (drawPlantUMLDiagram SVG) plantumlstring
      writeFilesToFolders folders B.writeFile svg "Diagram.svg"
      layout <- pickRandomLayout conf
      mapM_ (\(x,y) -> runExceptT $ cacheNet x (show . label) y False False True layout) $ zip folders petri
      writeFilesToFolders folders writeFile taskDescription  "TaskDescription.txt"
      writeFilesToFolders folders writeFile taskSolution "TaskSolution.txt"
    _ -> error "usage: one parameter required: FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

createExerciseFolders :: FilePath -> Int -> IO [FilePath]
createExerciseFolders path n = do
  let pathToFolders = map (\x -> path </> ("Exercise" ++ show x)) [1..n]
  mapM_ (createDirectoryIfMissing True) pathToFolders
  return $ map addTrailingPathSeparator pathToFolders

writeFilesToFolders :: [FilePath] -> (FilePath -> a -> IO()) -> [a] -> String -> IO ()
writeFilesToFolders folders writeFn files filename = do
  let paths = map (</> filename) folders
  mapM_ (uncurry writeFn) $ zip paths files
