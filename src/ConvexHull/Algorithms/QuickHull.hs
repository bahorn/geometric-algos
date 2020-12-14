{-# LANGUAGE NamedFieldPuns #-}

module ConvexHull.Algorithms.QuickHull (quickHull) where

import Data.List (sortOn)

import Trig.Polar (sortHull)
import Trig.Contains (inTriangle, segment, SegmentPosition (Above, Below, On))
import ConvexHull.Common

minX :: Points -> Point
minX = minimum

maxX :: Points -> Point
maxX = maximum

mid :: PointType -> PointType -> PointType
mid a b = a + (b - a) / 2

midpoint :: Point -> Point -> Point
midpoint Point { x = x1, y = y1 } Point { x = x2, y = y2 } = Point
  { x = mid x1 x2
  , y = mid y1 y2
  }

-- |Assigns it to either segment 1 or 2
whichSegment :: Point -> Point -> Point -> ([Point], [Point])
whichSegment a b p = case segment a b p of
  Above -> ([], [p])
  Below -> ([p], [])
  On    -> ([], [])

segmented :: Point -> Point -> Points -> (Points, Points)
segmented _ _ [] = ([], [])
segmented a b p =
    foldl1 (\(a1, b1) (a2, b2) -> (a1 ++ a2, b1 ++ b2))
        $ map (whichSegment a b) p

distanceLine :: Point -> Point -> Point -> PointType
distanceLine Point { x = x1, y = y1 } Point { x = x2, y = y2 } Point { x, y } =
    abs ((y2 - y1) * x - (x2 - x1) * y + x2*y1 - y2*x1)
        / sqrt ((y2 - y1)**2 + (x2 - x1)**2)

maxDistance :: Points -> Point -> Point -> Point
maxDistance points p1 p2 =
    head $ sortOn (negate . distanceLine p1 p2) points

findHull :: Points -> Point -> Point -> Points
findHull [] _ _ = []
findHull s a b =
    m : findHull left a m ++ findHull right m b
    where m = maxDistance s a b
          -- Remove points which can't be in the hull
          fixed = filter (\x -> x /= m && x /= a && x /= b) $
              filter (not . inTriangle a m b) s
          -- Splitting into two
          (left, right) = segmented (midpoint a b) m fixed

quickHull :: Points -> ConvexHull
quickHull p =
    sortHull . SimplePolygon $ a : b : findHull left a b ++ findHull right b a
    where a = minX p
          b = maxX p
          (left, right) = segmented a b $ filter (\x -> x /= a && x /= b) p
