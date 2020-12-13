{-# LANGUAGE NamedFieldPuns #-}

module Trig.Contains where

import ConvexHull.Common (Point (..))

data SegmentPosition = Above | Below | On
    deriving (Eq, Show, Enum)

data CCW = Clockwise | CounterClockwise | Collinear
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
segment Point { x = ax, y = ay } Point { x = bx, y = by } Point { x, y }
  | v > 0 = Above
  | v < 0 = Below
  | otherwise = On
  where v = (ax - x) * (by - y) - (bx - x) * (ay - y)

ccw :: Point -> Point -> Point -> CCW
ccw Point { x = x1, y = y1 } Point { x = x2, y = y2 } Point { x = x3, y = y3 }
  | res > 0 = CounterClockwise
  | res < 0 = Clockwise
  | res == 0 = Collinear
  where res = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 - x1))
