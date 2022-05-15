module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (writeFile)

import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)


import AD_Alloy (getRawAlloyInstances)
import AD_Instance (parseInstance)
import AD_Petrinet (convertToPetrinet, PetriKey(..))
import AD_PlantUMLConverter(convertToPlantUML)
import CallPlantUML(processPlantUMLString)

import Modelling.PetriNet.Diagram (cacheNet)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Control.Monad.Except(runExceptT)


main :: IO ()
main = do
  xs <- getArgs
  case xs of
    pathToJar:pathToFolder:xs' -> do
      inst <- getRawAlloyInstances (Just 50) 
      folders <- createExerciseFolders pathToFolder (length inst)
      writeFilesToFolders folders inst "Diagram.als"
      let ad = map (failWith id . parseInstance "this" "this" . failWith show . AD.parseInstance) inst
          plantumlstring = map convertToPlantUML ad
          petri = map convertToPetrinet ad
      svg <- mapM (`processPlantUMLString` pathToJar) plantumlstring
      writeFilesToFolders folders svg "Diagram.svg"
      mapM_ (\(x,y) -> runExceptT $ cacheNet x (show . label) y False False False Dot) $ zip folders petri
    _ -> error "usage: two parameters required: FilePath (PlantUML jar) FilePath (Output Folder)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

createExerciseFolders :: FilePath -> Int -> IO [FilePath]
createExerciseFolders path n = do
  let pathToFolders = map (\x -> path </> ("Exercise" ++ show x)) [1..n]
  mapM_ (createDirectoryIfMissing True) pathToFolders
  return $ map addTrailingPathSeparator pathToFolders

writeFilesToFolders :: [FilePath] -> [ByteString] -> String -> IO ()  
writeFilesToFolders folders files filename = do
  let paths = map (\x -> x </> filename) folders
  mapM_ (\(x,y) -> B.writeFile x y) $ zip paths files