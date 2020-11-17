module Main where

import MiscUtils.Generation (randomInput)
import Display.ConvexHull
import ConvexHull.Algorithms.QuickHull -- .GrahamScan 

main :: IO ()
main = do
    points <- randomInput 5 1000
    putStrLn $ "Running with: " ++ show points
    let hull = quickHull points
    print hull
    plotHull "test.png" points (hull ++ [head hull])
