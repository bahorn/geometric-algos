module ConvexHull.Common where


type PointType = Float

-- (X, Y)
type Point = (PointType, PointType)

-- Input
type Points = [Point]

-- (Just to specify when we return a ConvexHull or not.)
type ConvexHull = Points
