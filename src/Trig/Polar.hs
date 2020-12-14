{-# LANGUAGE NamedFieldPuns #-}

module Trig.Polar where

import Data.List (sortOn)
import ConvexHull.Common
  ( Point (..)
  , Points
  , PointType
  , ConvexHull
  , SimplePolygon (..)
  )

-- | atan2 requires floats :(
polarAngle :: Point -> PointType
polarAngle Point { x, y } = atan2 y x

polarAngleFrom :: Point -> Point -> PointType
polarAngleFrom Point { x = x1, y = y1 } Point { x = x2, y = y2 } =
  polarAngle Point { x = x2-x1, y = y2-y1 }

avgX :: Points -> PointType
avgX p = sum (map x p) / fromIntegral (length p)

avgY :: Points -> PointType
avgY p = sum (map y p) / fromIntegral (length p)

sortHull :: ConvexHull -> ConvexHull
sortHull (SimplePolygon p) = SimplePolygon $
  sortOn (polarAngleFrom Point { x = avgX p, y = avgY p }) p
