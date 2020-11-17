module Trig.Polar where

import Data.List (sortOn)
import ConvexHull.Common (Point, Points, PointType, ConvexHull)

-- | atan2 requires floats :(
polarAngle :: Point -> PointType
polarAngle (x, y) = atan2 y x

polarAngleFrom :: Point -> Point -> PointType
polarAngleFrom (x1, y1) (x2, y2) = polarAngle (x2-x1, y2-y1)

avgX :: Points -> PointType
avgX p = sum (map fst p) / fromIntegral (length p)

avgY :: Points -> PointType
avgY p = sum (map snd p) / fromIntegral (length p)

sortHull :: ConvexHull -> ConvexHull
sortHull p = sortOn (polarAngleFrom (avgX p, avgY p)) p
