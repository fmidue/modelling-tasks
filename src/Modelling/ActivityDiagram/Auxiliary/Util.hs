{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice,
  weightedShuffle
  ) where

import Control.Monad.Random (
  MonadRandom,
  fromList,
  )
import Data.List (delete)
import Data.String.Interpolate          (iii)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  )

{-|
  Shuffle a list of elements from type a based on given weights of type w,
  where higher weight indicates a bigger probability of the element occurring
  at a lower index of the list. The total weight of all elements must not be zero.
-}
weightedShuffle
  :: (MonadRandom m, Eq a, Real w)
  => [(a,w)]
  -> m [a]
weightedShuffle [] = return []
weightedShuffle xs = do
  let rs = map (\x -> (x, toRational $ snd x)) xs
  a <- fromList rs
  ys <- weightedShuffle (delete a xs)
  return (fst a : ys)

finalNodesAdvice :: OutputCapable m => Bool -> LangM m
finalNodesAdvice withFinalTransitionAdvice = do
  paragraph $ translate $ do
    english $ [iii|
      Hint on the translation to a Petri net:
      For final nodes no additional places are introduced.
      They are realised in a way that a token is consumed,
      i.e. disappears from the net at that position.
      |]
      `appendExtendedAdvice`
      [iii|
      If an additional transition is required to realise this behavior
      at a position in the diagram where there is a final node,
      this transition does not count as auxiliary node.
      |]
    german $ [iii|
      Hinweis zur Übersetzung in ein Petrinetz:
      Für Endknoten  werden keine zusätzlichen Stellen eingeführt.
      Sie werden so realisiert, dass ein Token verbraucht wird,
      also an dieser Position aus dem Netz verschwindet.
      |]
      `appendExtendedAdvice`
      [iii|
      Falls eine zusätzliche Transition erforderlich ist,
      um dieses Verhalten an einer Position im Diagramm zu realisieren,
      an der sich ein Endknoten befindet,
      zählt diese Transition nicht als Hilfsknoten.
      |]
  pure ()
  where
    appendExtendedAdvice x y
      | withFinalTransitionAdvice = x ++ ' ' : y
      | otherwise = x
