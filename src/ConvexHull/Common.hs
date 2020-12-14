module ConvexHull.Common where


type PointType = Float

-- (X, Y)
data Point = Point { x :: PointType, y :: PointType } deriving (Show, Eq, Ord)

-- Input
type Points = [Point]

-- (Just to specify when we return a ConvexHull or not.)
newtype SimplePolygon = SimplePolygon { points :: Points } deriving (Show, Eq)
type ConvexHull = SimplePolygon
