module MiscUtils.Generation where

import System.Random

import ConvexHull.Common (Point (), Points (), PointType ())

randomPointType :: IO PointType
randomPointType = randomIO :: IO PointType

randomPoint :: IO Point
randomPoint = do
    x <- randomPointType
    y <- randomPointType
    return (x, y)

randomInput :: IO Points
randomInput = do
    count <- randomRIO (5, 100) :: IO Int
    mapM (const randomPoint) [1..count]
