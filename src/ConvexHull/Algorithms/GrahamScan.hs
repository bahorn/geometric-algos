module ConvexHull.Algorithms.GrahamScan (grahamScan, findP0) where

import Data.List (nub, sortOn)

import Trig.Contains (ccw, CCW (CounterClockwise))
import Trig.Polar
import ConvexHull.Common

type Stack = [Point]

-- |Finds the lowest y-coordinate and leftmost point.
findP0 :: Points -> Point
findP0 [a] = a
findP0 z = do
    let (y', x') = minimum $ map (\(x, y) -> (y, x)) z
    (x', y')

sortAngle :: Point -> Points -> Points
sortAngle p = sortOn (polarAngleFrom p)

stackLoop :: Stack -> Point -> Stack
stackLoop [] _ = []
stackLoop all@(s : stack) point
    | length stack <= 1 = all
    | CounterClockwise == ccw (head stack) s point = all
    | otherwise = stackLoop stack point

grahamLoop :: (Stack, Points) -> Stack
grahamLoop (stack, []) = stack
grahamLoop (stack, p : points) =
    grahamLoop (p : stackLoop stack p, points)

-- |Setup the everything for computing Graham's Scan
grahamInit :: Points -> (Stack, Points)
grahamInit points = ([], sortAngle (findP0 points) points)

-- |Compute the ConvexHull of a set of points using a Graham's Scan
grahamScan :: Points -> ConvexHull
grahamScan p = sortHull (grahamLoop $ grahamInit (nub p))
