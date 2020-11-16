module Display.ConvexHull where

import qualified Graphics.Rendering.Chart.Easy as C
import qualified Graphics.Rendering.Chart.Backend.Cairo as C

import ConvexHull.Common (Points (), ConvexHull ())

plotHull :: String -> Points -> ConvexHull -> IO ()
plotHull fname points hull = do
    let def = C.def
    C.toFile def fname $ do
        C.layout_title C..= "Hull Plot"
        -- C.layout_x_axis . C.laxis_generate C..= C.scaledAxis C.def (-0.25, 1.25)
        -- C.layout_y_axis . C.laxis_generate C..= C.scaledAxis C.def (-0.25, 1.25)
        C.plot $ C.points "" points
        C.plot $ C.line "" [hull]
