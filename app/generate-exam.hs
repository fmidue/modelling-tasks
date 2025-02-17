{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Csv                         as Csv
import qualified Data.Vector                      as V (Vector)

import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.DifferentNames    (differentNamesTask)
import Modelling.CdOd.MatchCdOd         (matchCdOdTask)
import Modelling.CdOd.RepairCd          (repairCdTask)
import Modelling.CdOd.SelectValidCd     (selectValidCdTask)
import Modelling.PetriNet.Concurrency (
  simpleFindConcurrencyTask,
  )
import Modelling.PetriNet.Conflict (
  simpleFindConflictTask,
  )
import Modelling.PetriNet.ConflictPlaces (simpleFindConflictPlacesTask)
import Modelling.PetriNet.MatchToMath   (graphToMathTask, mathToGraphTask)
import Modelling.PetriNet.Reach.Deadlock (deadlockTask, toShowDeadlockInstance)
import Modelling.PetriNet.Reach.Reach   (reachTask, toShowReachInstance)
--import Modelling.PetriNet.Types         (SimpleNode)

import Control.Monad                    (foldM_, void, when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.OutputCapable.Blocks (
  GenericLangM (unLangM),
  LangM',
  Language (English, German),
  ReportT,
  )
import Control.OutputCapable.Blocks.LaTeX       (toLaTeX)
import Data.Typeable                    (Typeable, typeOf)
import Data.IORef                       (IORef, newIORef, readIORef, writeIORef)
import Data.Text                        (pack, unpack)
import GHC.IO.Unsafe                    (unsafePerformIO)
import Text.LaTeX.Base.Render           (Render (render))
import Text.LaTeX.Base.Syntax (
  LaTeX (TeXComm, TeXCommS, TeXEnv, TeXRaw),
  TeXArg (FixArg, OptArg),
  )
import System.Directory                 (createDirectoryIfMissing)
import System.Environment               (getArgs)
import Text.Read                        (readMaybe)

--instance deriving Data FindInstance
--instance deriving Data Conflict
--instance deriving Data ReachInstance
--instance deriving Data DeadlockInstance

{-# NOINLINE output #-}
output :: IORef Bool
output = unsafePerformIO (newIORef True)

{-# NOINLINE tasks #-}
tasks :: IORef [String]
tasks = unsafePerformIO (newIORef [])

newline :: LaTeX
newline = TeXRaw "\n"

main :: IO ()
main = do
  [ts,p] <- getArgs
  writeIORef output $ read p
  writeIORef tasks $ read ts
  putStrLn $ "Tasks: " ++ ts
  let defaultLayout d =
        TeXComm "documentclass" [
          OptArg $ TeXRaw $ pack "a4paper,12pt",
          FixArg $ TeXRaw $ pack "article"]
        <> newline
        <> TeXComm "usepackage" [
          OptArg $ TeXRaw $ pack "export",
          FixArg $ TeXRaw $ pack "adjustbox"]
        <> newline
        <> TeXComm "RequirePackage" [FixArg $ TeXRaw $ pack "figureSeries"]
        <> newline
        <> TeXComm "renewcommand" [
          FixArg $ TeXCommS "labelitemi",
          FixArg $ TeXRaw $ pack "--"
          ]
        <> newline
        <> TeXEnv "document" [] (newline <> d)
  let latex = unpack . render . defaultLayout
  let tasksToBoth f tid = do
        void $ tasksToLatex f tid English
        void $ tasksToLatex f tid German
  void $ tasksToBoth selectValidCdTask "132"
  void $ tasksToBoth selectValidCdTask "133"
  void $ tasksToBoth repairCdTask "134"
  void $ tasksToBoth differentNamesTask "135"
  void $ tasksToBoth differentNamesTask "136"
  void $ tasksToBoth matchCdOdTask "137"
  void $ tasksToBoth matchCdOdTask "138"
  void $ tasksToBoth mathToGraphTask "139"
  void $ tasksToBoth graphToMathTask "140"
  void $ tasksToBoth simpleFindConcurrencyTask "141"
  void $ tasksToBoth simpleFindConcurrencyTask "142"
  void $ tasksToBoth simpleFindConflictTask "143"
  void $ tasksToBoth simpleFindConflictPlacesTask "144"
  let reachTask' f = reachTask f . toShowReachInstance
  void $ tasksToBoth reachTask' "145"
  let deadlockTask' f = deadlockTask f . toShowDeadlockInstance
  void $ tasksToBoth deadlockTask' "146"
  putStrLn $ latex $ TeXRaw $ pack ""

type InstanceCSV = (Int, Int, FilePath)

tasksToLatex
  :: (Typeable t, MonadIO m, Read t, Show t)
  => (FilePath -> t -> LangM' (ReportT LaTeX IO) b)
  -> FilePath
  -> Language
  -> m ()
tasksToLatex ftask taskId l = do
  f <- liftIO $ LBS.readFile $ taskId ++ ".csv"
  let csv = either error (id @(V.Vector InstanceCSV)) $ Csv.decode Csv.NoHeader f
  ts <- liftIO $ readIORef tasks
  when (taskId `elem` ts) $ do
    foldM_
      (\latex (_, mnr, inst) ->
         (latex <>) <$> taskToLatex ftask mnr taskId l inst)
      mempty
      csv

copyToFiles
  :: (MonadIO m, Render b, Show a)
  => String
  -> a
  -> b
  -> m b
copyToFiles prefix t latex = do
  liftIO . writeFile (folder ++ prefix ++ ".hs") $ show t
  copyToFile prefix latex

copyToFile :: (MonadIO m, Render b) => String -> b -> m b
copyToFile prefix latex = do
  liftIO . writeFile (folder ++ prefix ++ ".tex") . unpack $ render latex
  return latex

folder :: FilePath
folder = "exam/"

taskToLatex
  :: (Typeable t, MonadIO m, Read t, Show t)
  => (FilePath -> t -> LangM' (ReportT LaTeX IO) b)
  -> Int
  -> FilePath
  -> Language
  -> FilePath
  -> m ()
taskToLatex ftask mnr taskId l inst = do
 liftIO $ createDirectoryIfMissing True (folder ++ show l ++ '/' : taskId)
 let name = show l ++ '/' : taskId ++ '/' : show mnr
 liftIO $ putStrLn name
 o <- liftIO $ readIORef output
 when o $ do
  t' <- liftIO $ readDebug ("instance " ++ "i" ++ tail inst) <$> readFile ("./i" ++ tail inst)
  taskLatex <- liftIO $ ($ l) <$> toLaTeX (unLangM $ ftask (folder ++ name) t')
  let latexS = newpage
        <> newline
        <> TeXComm "subsection*" [FixArg $ TeXRaw $ pack $
                              "task (" ++ taskId ++ ")"]
        <> newline
        {-
        <> TeXComm "url" [
          FixArg $ TeXRaw $ pack
            $ "https://autotool.fmi.uni-due.de/route/aufgabe/"
            ++ taskId ++ "/einsendung"]
        <> newline
        -}
      latex = latexS <> taskLatex <> newpage <> newline
  void $ copyToFiles name t' latex

readDebug :: forall p . (Read p, Typeable p) => String -> String -> p
readDebug dbg x = case readMaybe x of
  Just y -> y
  Nothing -> error $ unlines [show x, show (typeOf @p undefined), dbg, "parsing failed!"]

newpage :: LaTeX
newpage = TeXComm "newpage" []
