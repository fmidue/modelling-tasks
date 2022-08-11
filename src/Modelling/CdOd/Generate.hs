module Modelling.CdOd.Generate (
  generateCds,
  instanceToEdges,
  nameEdges,
  ) where

import qualified Data.Bimap                       as BM (
  fromList,
  keysR,
  )

import Modelling.CdOd.Auxiliary.Util    (getInstances)
import Modelling.CdOd.CdAndChanges.Instance (
  fromInstance,
  )
import Modelling.CdOd.CdAndChanges.Transform (
  transformNoChanges,
  )
import Modelling.CdOd.Edges             (
  renameClasses,
  )
import Modelling.CdOd.Types (
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  RelationshipProperties,
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (MonadRandom)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

generateCds
  :: (MonadIO m, MonadRandom m)
  => Maybe Bool
  -> ClassConfig
  -> RelationshipProperties
  -> Maybe Integer
  -> Maybe Int
  -> m [AlloyInstance]
generateCds withNonTrivialInheritance config props maxInsts to = do
  let alloyCode = transformNoChanges config props withNonTrivialInheritance
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  shuffleM instas

instanceToEdges :: AlloyInstance -> Either String ([String], [DiagramEdge])
instanceToEdges rinsta = do
  ((cs, _), es, []) <- fromInstance rinsta
  let cns = BM.fromList $ zip cs $ map pure ['A'..]
  return (BM.keysR cns, nameEdges $ renameClasses cns es)

nameEdges :: [DiagramEdge] -> [DiagramEdge]
nameEdges es =
     [e | e@(_, _, Inheritance) <- es]
  ++ [(s, e, Assoc k [n] m1 m2 b)
     | (n, (s, e, Assoc k _ m1 m2 b)) <- zip ['z', 'y' ..] es]
