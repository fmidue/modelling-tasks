{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Step.hs
-}
module Modelling.PetriNet.Reach.Step where

import qualified Data.Map                         as M (
  insert,
  findWithDefault,
  )

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Type (
  Net (capacity, connections, start),
  State (State),
  allNonNegative,
  conforms,
  )

import Control.Applicative              (Alternative)
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (unless)
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (image, indent, paragraph, refuse, text),
  LangM',
  OutputCapable,
  ($=<<),
  english,
  german,
  recoverWith,
  translate,
  unLangM,
  )
import Control.OutputCapable.Blocks.Generic (
  ($>>=),
  )
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable                    (Foldable (foldl'))
#endif
import Control.Monad                    (foldM)
import Data.GraphViz                    (GraphvizCommand)
import Data.Maybe                       (listToMaybe)

equalling :: Eq a => (t -> a) -> t -> t -> Bool
equalling f x y = f x == f y

--executesPlain n ts = result $ executes n ts

executes
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputCapable m,
    Show s,
    Show t
    )
  => FilePath
  -> GraphvizCommand
  -> Net s t
  -> [t]
  -> LangM' m (Either Int (State s))
executes path cmd n ts = foldl'
  (\z (k, t) -> either (pure . Left) (step k t) $=<< unLangM z)
  (pure $ Right $ start n)
  (zip [1 :: Int ..] ts)
  where
    step k t z = recoverWith (k - 1) $ do
      paragraph $ translate $ do
        english $ "Step " ++ show k
        german $ "Schritt " ++ show k
      next <- executeIO path cmd n t z
      pure next

executeIO
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord a,
    Ord k,
    OutputCapable m,
    Show a,
    Show k
    )
  => FilePath
  -> GraphvizCommand
  -> Net k a
  -> a
  -> State k
  -> LangM' m (State k)
executeIO path cmd n t z0 = execute n t z0
  $>>= \z2 -> lift (drawToFile False True path cmd (n {start = z2}))
  $>>= \g -> image g
  $>>= pure (pure z2)

execute
  :: (Monad m, Ord a, Ord k, OutputCapable m, Show a, Show k)
  => Net k a
  -> a
  -> State k
  -> LangM' m (State k)
execute n t z0 = do
  paragraph (text $ "Transition " ++ show t)
  next <- case cs of
    [] -> do
      refuse $ translate $ do
        english "does not exist!"
        german "existiert nicht!"
      pure z0
    [(vor, _, nach)] -> do
      let z1 = change pred vor z0
      paragraph $ translate $ do
        english "Intermediate marking (after collecting tokens)"
        german "Zwischenmarkierung (nach Einziehen der Marken im Vorbereich)"
      indent $ text $ show z1
      unless (allNonNegative z1) $ refuse $ paragraph $ translate $ do
        english "Contains negative token count (transition was not activated)!"
        german "Enthält negative Markenanzahl (Transition war nicht aktiviert)!"
      let z2 = change succ nach z1
      paragraph $ translate $ do
        english "Final marking (after distributing tokens)"
        german "Endmarkierung (nach Austeilen der Marken im Nachbereich)"
      indent $ text $ show z2
      unless (conforms (capacity n) z2) $ refuse $ paragraph $ translate $ do
        english "contains more tokens than capacity permits!"
        german "enthält mehr Marken, als die Kapazität zulässt!"
      pure z2
    _ -> undefined -- TODO: Pattern match not required?
  pure next
  where
    cs = [ c | c@(_, t', _) <- connections n, t' == t]

successors
  :: Ord s
  => Net s t
  -> State s
  -> [(t, State s)]
successors n z0 = [ (t, z2) |
    (vor, t, nach) <- connections n,
    let z1 = change pred vor z0,
    allNonNegative z1,
    let z2 = change succ nach z1,
    conforms (capacity n) z2
  ]

fireTransition :: (Eq t, Ord s) => Net s t -> t -> State s -> Maybe (State s)
fireTransition n theTransition z0 = listToMaybe [ z2
  | (vor, t, nach) <- connections n
  , t == theTransition
  , let z1 = change pred vor z0
  , allNonNegative z1
  , let z2 = change succ nach z1
  , conforms (capacity n) z2
  ]

executeSequence :: (Eq t, Ord s) => Net s t -> [t] -> Maybe (State s)
executeSequence n = foldM (flip (fireTransition n)) (start n)

change
  :: Ord s
  => (Int -> Int)
  -> [s]
  -> State s
  -> State s
change f ps (State z) =
  State $ foldl
    (\z' p -> M.insert p (f $ M.findWithDefault 0 p z') z')
    z
    ps
