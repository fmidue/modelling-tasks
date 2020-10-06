-- | Common functions for application modules
module Common (
  forceErrors, printNetAndInfo, renderPetriNet,
  ) where

import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Diagrams.Prelude                 (Diagram, mkWidth)
import Diagrams.Backend.SVG             (B, renderSVG)

forceErrors :: Monad m => ExceptT String m () -> m ()
forceErrors m = do
  () <- either error id <$> runExceptT m
  return ()

printNetAndInfo :: Show a => String -> (Diagram B, a) -> IO ()
printNetAndInfo x (dia, concurrent) = do
  renderPetriNet x dia
  print concurrent

renderPetriNet :: String -> Diagram B -> IO ()
renderPetriNet x dia = do
  renderSVG name (mkWidth 800) dia
  putStrLn $ "wrote file" ++ name
  where
    name = x ++ "petri.svg"
