module Trig.Contains where

import ConvexHull.Common (Point)

data SegmentPosition = Above | Below | On
    deriving (Eq, Show, Enum)

-- |Given 3 points defining a triangle, return if another point is inside of it.
inTriangle :: Point -> Point -> Point -> Point -> Bool
inTriangle a b c p
  | a' == Above && b' == Above && c' == Above = True
  | otherwise = False
  where a' = segment a b p
        b' = segment b c p
        c' = segment c a p

-- |Specify whenever this point is below or above the line between two other
-- points.
segment :: Point -> Point -> Point -> SegmentPosition
segment (ax, ay) (bx, by) (x, y)
  | v > 0 = Above
  | v < 0 = Below
  | otherwise = On
  where v = (ax - x) * (by - y) - (bx - x) * (ay - y) 
