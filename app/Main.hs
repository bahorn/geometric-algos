module Main where

import MiscUtils.Generation (randomInput)
import Display.Display
import Display.ConvexHull
import ConvexHull.Algorithms.QuickHull
import ConvexHull.Algorithms.GrahamScan
import ConvexHull.Algorithms.JarvisMarch
import qualified Graphics.Gloss as Gloss

main :: IO ()
main = do
    points <- randomInput 5 1000
    --putStrLn $ "Running with: " ++ show points
    let hull1 = quickHull points
    let hull2 = grahamScan points
    let hull3 = jarvisMarch points
    print $ hull1 == hull2
    print hull3
    toScreen (Gloss.pictures [render hull1, render points])
