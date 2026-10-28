module Modelling.CdOd.OutputSpec where

import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstance,
  )

import qualified Data.ByteString.Char8            as BS (
  pack,
  readFile,
  writeFile,
  unpack
  )
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.Output            (drawCd, drawOd)
import Modelling.CdOd.Types (
  anonymiseObjects,
  defaultCdDrawSettings,
  )
import Modelling.Common                 (withUnitTestsUsingPath)

import Control.Monad.Except             (runExceptT)
import Control.Monad.Random             (evalRandT)
import Data.GraphViz                    (DirType (Forward))
import Test.Hspec                       (Spec)
import Test.Similarity                  (Deviation (..), shouldReturnSimilar)
import System.IO.Extra                  (withTempFile)
import System.Random                    (mkStdGen)
import Language.Alloy.Debug             (parseInstance)

spec :: Spec
spec = do
  withUnitTestsUsingPath "drawCd" (draws "class") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . fmap BS.unpack . drawCdInstance
  withUnitTestsUsingPath "drawOd" (draws "object") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . fmap BS.unpack . drawOdInstance
  where
    shouldReturnSimilar' f = shouldReturnSimilar
      f
      200
      Deviation {absoluteDeviation = 20, relativeDeviation = 0.2}
    draws what = "draws roughly the expected " ++ what ++ " diagram"
    dir = "test/unit/Modelling/CdOd/Output"
    drawCdInstance alloy = do
      Right alloyInstance <- runExceptT $ parseInstance (BS.pack alloy)
      Right cd <- return $ instanceClassDiagram <$> fromInstance alloyInstance
      fileCreationWith $ drawCd defaultCdDrawSettings mempty Nothing cd
    drawOdInstance alloy = do
      Right alloyInstance <- runExceptT $ parseInstance (BS.pack alloy)
      let possibleLinks = map (: []) ['w'..'y']
      fileCreationWith $ do
        od <- alloyInstanceToOd Nothing possibleLinks alloyInstance
        od' <- evalRandT (anonymiseObjects 1 od) $ mkStdGen 0
        drawOd od' Nothing Forward True
    fileCreationWith action = withTempFile $ \file ->
      action >>= BS.writeFile file >> BS.readFile file
