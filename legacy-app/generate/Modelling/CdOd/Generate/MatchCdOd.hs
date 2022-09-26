{-# OPTIONS_GHC -Wwarn=deprecations #-}
module Modelling.CdOd.Generate.MatchCdOd (
  matchCdOd,
  ) where


import qualified Data.Map                         as M (
  fromList,
  )

import Modelling.CdOd.Auxiliary.Util (
  redColor,
  )
import Modelling.CdOd.Edges (
  DiagramEdge,
  anyMarkedEdge,
  checkMultiEdge,
  fromEdges,
  )
import Modelling.CdOd.Generate.Generate (generate)
import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance,
  getMatchCdOdTask,
  getODInstances,
  takeRandomInstances,
  )
import Modelling.CdOd.Generate.Mutation
  (Target (..), getAllMutationResults, nonTargets)
import Modelling.CdOd.Output
  (drawCdFromSyntax)
import Modelling.CdOd.Types (
  Syntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Data.GraphViz                    (GraphvizOutput (Pdf))
import Data.Map                         (Map)
import Data.Set                         (singleton)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

matchCdOd :: MatchCdOdConfig -> Int -> Int -> IO MatchCdOdInstance
matchCdOd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchCdOdTask getRandomTask config) g

getRandomTask
  :: RandomGen g
  => MatchCdOdConfig
  -> RandT g IO (Map Int Syntax, Map Char ([Int], AlloyInstance))
getRandomTask config = do
  (cd1, cd2, cd3, numClasses) <- getRandomCDs config
  instas <- liftIO $ getODInstances config cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getRandomTask config
    Just rinstas -> return (M.fromList [(1, cd1), (2, cd2)], M.fromList $ zip ['a' ..] rinstas)

getRandomCDs :: RandomGen g => MatchCdOdConfig -> RandT g IO (Syntax, Syntax, Syntax, Int)
getRandomCDs config = do
  (names, edges) <- generate
    Nothing
    (classConfig config)
    (searchSpace config)
  let cd0 = fromEdges names edges
  -- continueIf (not (anyMarkedEdge cd0)) $ do
  when debug . liftIO . void $ drawCdFromSyntax False True (Just redColor) cd0 "debug-0" Pdf
  mutations <- shuffleM $ getAllMutationResults (classConfig config) names edges
  let medges1 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations
  continueWithJust medges1 (const True) $ \edges1 -> do
    mutations' <- shuffleM mutations
    let medges2     = getFirstValidSatisfying (const True) names mutations'
        notOnlyInhs = not . null . nonTargets (singleton TInheritance) . (edges1 ++)
    continueWithJust medges2 notOnlyInhs $ \edges2 -> do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let medges3 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations''
      continueWithJust medges3 (const True) $ \edges3 -> do
        let cd3         = fromEdges names edges3
        when debug . void . liftIO
          $ drawCdFromSyntax False True (Just redColor) cd3 "debug-3" Pdf
        return (cd1, cd2, cd3, length names)
  where
    continueWithJust mx p m
      | Just x <- mx, p x = m x
      | otherwise         = getRandomCDs config

getFirstValidSatisfying
  :: (Syntax -> Bool) -> [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValidSatisfying _ _     []
  = Nothing
getFirstValidSatisfying p names (x:xs)
  | checkMultiEdge x, p (fromEdges names x)
  = Just x
  | otherwise
  = getFirstValidSatisfying p names xs