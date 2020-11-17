module Trig.Polar where

import ConvexHull.Common (Point, PointType)

-- | atan2 requires floats :(
polarAngle :: Point -> PointType
polarAngle (x, y) = atan2 y x

polarAngleFrom :: Point -> Point -> PointType
polarAngleFrom (x1, y1) (x2, y2) = polarAngle (x2-x1, y2-y1)
