module Main where

import MiscUtils.Generation (randomInput)
import Display.ConvexHull
import ConvexHull.Algorithms.QuickHull
import ConvexHull.Algorithms.GrahamScan 

main :: IO ()
main = do
    points <- randomInput 5 1000
    --putStrLn $ "Running with: " ++ show points
    let hull1 = quickHull points
    let hull2 = grahamScan points
    print $ hull1 == hull2
    plotHull "test.png" points (hull1 ++ [head hull1])
