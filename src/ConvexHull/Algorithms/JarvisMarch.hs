module ConvexHull.Algorithms.JarvisMarch where

import Trig.Contains (ccw, CCW (CounterClockwise))
import ConvexHull.Common

minX :: Points -> Point
minX = minimum

-- This finds an endpoint
innerLoop :: Point -> Points -> Point -> Point
innerLoop _ [] end = end 
innerLoop ph (p : rest) end
  | p == ph = innerLoop ph rest p
  | CounterClockwise == ccw ph end p = innerLoop ph rest p
  | otherwise = innerLoop ph rest end

jarvisLoop :: ConvexHull -> Points -> ConvexHull
jarvisLoop hull@(first : _) points@(p : _)
  | endpoint == first = hull
  | otherwise = jarvisLoop (hull ++ [endpoint]) points
  where endpoint = innerLoop first points p

jarvisMarch :: Points -> ConvexHull
jarvisMarch p = do
    let pointOnHull = minX p
    pointOnHull : jarvisLoop [pointOnHull] p
