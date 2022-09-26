module Main where

import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)

import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.EnterAS (EnterASInstance(..), defaultEnterASConfig, enterASAlloy, checkEnterASInstance, enterActionSequenceText, enterASTaskDescription)
import Modelling.ActivityDiagram.PlantUMLConverter(convertToPlantUML)
import Language.Alloy.Call (getInstances)
import Language.PlantUML.Call (DiagramType(SVG), drawPlantUMLDiagram)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToFolder:xs' -> do
      inst <- getInstances (Just 50) $ enterASAlloy defaultEnterASConfig
      let ad = map (failWith id . parseInstance) inst
          enterAS = map enterActionSequenceText
                    $ filter (isNothing . (`checkEnterASInstance` defaultEnterASConfig))
                    $ map (\x -> EnterASInstance{activityDiagram = x, seed=123}) ad
          plantumlstring = map (convertToPlantUML . fst) enterAS
          taskDescription = replicate (length enterAS) enterASTaskDescription
          taskSolution = map snd enterAS
      folders <- createExerciseFolders pathToFolder (length enterAS)
      svg <- mapM (drawPlantUMLDiagram SVG) plantumlstring
      writeFilesToFolders folders B.writeFile svg "Diagram.svg"
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