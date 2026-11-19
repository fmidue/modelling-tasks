module Modelling.Auxiliary.DiagramsSpec where

import Modelling.Auxiliary.Diagrams     (pointAtShiftedBy)

import Diagrams.Prelude (
  Located,
  P2,
  Trail,
  V2 (V2),
  at,
  bezier3,
  fromOffsets,
  fromSegments,
  origin,
  p2,
  unp2,
  )
import Test.Hspec
import Test.HUnit                       (assertBool)

spec :: Spec
spec =
  describe "pointAtShiftedBy" $ do
    it "shifts perpendicular to a straight shaft" $
      pointAtShiftedBy straight 0.5 (V2 3 2) 0
        `shouldBeCloseTo` p2 (5, 2)
    it "treats a zero-length segment like the surrounding shaft" $
      pointAtShiftedBy zeroLengthMiddle 0.5 (V2 3 2) 0
        `shouldBeCloseTo` p2 (5, 2)
    it "applies the additional distance at a zero-length segment" $
      pointAtShiftedBy zeroLengthMiddle 0.5 (V2 3 2) 2
        `shouldBeCloseTo` p2 (5, 4)
    it "uses the overall direction at a cusp, where the tangent vanishes" $
      pointAtShiftedBy cusp 0.5 (V2 3 2) 0
        `shouldBeCloseTo` p2 (2.5, 9.5)
    it "handles a real Graphviz edge having control points on a segment start" $
      shouldBeWithin 0.01
        (pointAtShiftedBy graphvizEdge 0.4 (V2 3 2) 0)
        (p2 (340.1658, 126.7976))
    it "does not shift at all on a shaft that is a single point" $
      pointAtShiftedBy singlePoint 0.5 (V2 3 2) 2
        `shouldBeCloseTo` p2 (1, 1)

{-|
From (0, 0) to (10, 0).
-}
straight :: Located (Trail V2 Double)
straight = fromOffsets [V2 10 0] `at` origin

{-|
From (0, 0) to (10, 0), but parameter 0.5 lies on a segment of length zero.
-}
zeroLengthMiddle :: Located (Trail V2 Double)
zeroLengthMiddle = fromOffsets [V2 5 0, V2 0 0, V2 5 0] `at` origin

{-|
A Bézier curve from (0, 0) to (20, 0) whose derivative is zero at parameter 0.5
(where it is at (2.5, 7.5)), moving in positive x direction around there.
-}
cusp :: Located (Trail V2 Double)
cusp = fromSegments [bezier3 (V2 10 10) (V2 (-10) 10) (V2 20 0)] `at` origin

singlePoint :: Located (Trail V2 Double)
singlePoint = fromOffsets [V2 0 0] `at` p2 (1, 1)

{-|
An edge as laid out by Graphviz for a class diagram (taken from a rendered SVG).
Both control points of the third segment coincide with its starting point,
so the tangent vanishes exactly there, i.e. at parameter 0.4,
which is where the label of a forward directed edge is placed.
That point is (343.1642, 126.8633),
the edge is heading almost straight in positive y direction there.
-}
graphvizEdge :: Located (Trail V2 Double)
graphvizEdge = fromSegments [
  bezier3 (V2 7.3028 6.1661) (V2 15.1984 14.0386) (V2 20.3936 22.7080),
  bezier3 (V2 13.8823 23.1402) (V2 16.4318 31.7212) (V2 15.5304 58.7029),
  bezier3 (V2 0 0) (V2 0 0) (V2 (-4.9720) 148.8299),
  bezier3 (V2 (-0.9010) 26.9705) (V2 0.6789 39.5159) (V2 (-19.4133) 57.5293),
  bezier3 (V2 (-24.0591) 21.5666) (V2 (-59.9489) 28.8922) (V2 (-88.9907) 30.9393)
  ] `at` p2 (307.2402, 45.4524)

shouldBeCloseTo :: P2 Double -> P2 Double -> Expectation
shouldBeCloseTo = shouldBeWithin 1e-6

shouldBeWithin :: Double -> P2 Double -> P2 Double -> Expectation
shouldBeWithin tolerance actual expected = assertBool
  ("expected: " ++ show expected ++ "\n but got: " ++ show actual)
  $ abs (x - x') < tolerance && abs (y - y') < tolerance
  where
    (x, y) = unp2 actual
    (x', y') = unp2 expected
