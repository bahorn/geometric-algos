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

randomInput :: Int -> Int -> IO Points
randomInput start end = do
    count <- randomRIO (start, end) :: IO Int
    mapM (const randomPoint) [1..count]
