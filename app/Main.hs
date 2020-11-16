module Main where

import MiscUtils.Generation (randomInput)
import Display.ConvexHull
import ConvexHull.Algorithms.GrahamScan 
import ConvexHull.Common (Point (), Points (), PointType ())

main :: IO ()
main = do
    points <- randomInput
    let hull = grahamScan points
    plotHull "test.png" points (hull ++ [head hull])
