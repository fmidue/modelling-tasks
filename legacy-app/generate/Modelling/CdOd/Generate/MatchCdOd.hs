module Modelling.CdOd.Generate.MatchCdOd (
  matchCdOd,
  ) where


import qualified Data.Map                         as M (
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy)
import Modelling.Auxiliary.Common       (randomise)
import Modelling.CdOd.Generate.Edges (
  DiagramEdge,
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
import Modelling.CdOd.Types (
  Cd,
  anyThickEdge,
  )

import Control.Monad.Catch              (MonadCatch)
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Data.Map                         (Map)
import Data.Set                         (singleton)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

matchCdOd
  :: (MonadAlloy m, MonadCatch m, MonadFail m)
  => Int
  -> MatchCdOdConfig
  -> Int
  -> Int
  -> m MatchCdOdInstance
matchCdOd searchSpace config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (`evalRandT` g) $ do
    inst <- getMatchCdOdTask (getRandomTask searchSpace) config
    randomise inst

getRandomTask
  :: (MonadAlloy m, MonadFail m, RandomGen g)
  => Int
  -> MatchCdOdConfig
  -> RandT g m (Map Int Cd, Map Char ([Int], AlloyInstance))
getRandomTask searchSpace config = do
  (cd1, cd2, cd3, numClasses) <- getRandomCDs searchSpace config
  alloyInstances <- getODInstances config cd1 cd2 cd3 numClasses
  maybeRandomInstances <- takeRandomInstances alloyInstances
  case maybeRandomInstances of
    Nothing      -> getRandomTask searchSpace config
    Just randomInstances -> pure
      (M.fromList [(1, cd1), (2, cd2)], M.fromList $ zip ['a' ..] randomInstances)

getRandomCDs
  :: (MonadFail m, RandomGen g)
  => Int
  -> MatchCdOdConfig
  -> RandT g m (Cd, Cd, Cd, Int)
getRandomCDs searchSpace config = do
  (names, edges) <- generate
    Nothing
    (classConfig config)
    searchSpace
  --let cd0 = fromEdges names edges
  -- continueIf (not (anyThickEdge cd0)) $ do
  mutations <- shuffleM $ getAllMutationResults (classConfig config) names edges
  let medges1 = getFirstValidSatisfying (not . anyThickEdge) names mutations
  continueWithJust medges1 (const True) $ \edges1 -> do
    mutations' <- shuffleM mutations
    let medges2     = getFirstValidSatisfying (const True) names mutations'
        notOnlyInhs = not . null . nonTargets (singleton TInheritance) . (edges1 ++)
    continueWithJust medges2 notOnlyInhs $ \edges2 -> do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let medges3 =
            getFirstValidSatisfying (not . anyThickEdge) names mutations''
      continueWithJust medges3 (const True) $ \edges3 -> do
        let cd3         = fromEdges names edges3
        return (cd1, cd2, cd3, length names)
  where
    continueWithJust mx p m
      | Just x <- mx, p x = m x
      | otherwise         = getRandomCDs searchSpace config

getFirstValidSatisfying
  :: (Cd -> Bool) -> [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValidSatisfying _ _     []
  = Nothing
getFirstValidSatisfying p names (x:xs)
  | checkMultiEdge x, p (fromEdges names x)
  = Just x
  | otherwise
  = getFirstValidSatisfying p names xs
