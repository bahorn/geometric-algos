module Main where

import qualified Graphics.Rendering.Chart.Easy as C
import Graphics.Rendering.Chart.Backend.Cairo as C

import ConvexHull.Algorithms.GrahamScan 
import ConvexHull.Common (Point (), Points (), PointType (), ConvexHull ())
import System.Random

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


plotPoints :: String -> Points -> ConvexHull -> IO ()
plotPoints fname points hull = do
    let def = C.def
    C.toFile def fname $ do
        C.layout_title C..= "Hull Plot"
        C.layout_x_axis . C.laxis_generate C..= C.scaledAxis C.def (-0.25, 1.25)
        C.layout_y_axis . C.laxis_generate C..= C.scaledAxis C.def (-0.25, 1.25)
        C.plot $ C.points "" points
        C.plot $ C.line "" [hull]

main :: IO ()
main = do
    points <- randomInput
    let hull = grahamScan points
    plotPoints "test.png" points (hull ++ [head hull])
