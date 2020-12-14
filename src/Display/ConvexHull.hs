{-# LANGUAGE NamedFieldPuns #-}

module Display.ConvexHull where

import qualified Graphics.Gloss as Gloss

import ConvexHull.Common (Point (..), SimplePolygon (..))
import Display.Display

instance Display Point where
  render Point { x, y } =
    Gloss.color Gloss.black
      . Gloss.translate (100 * x) (100 * y)
      $ Gloss.circleSolid 0.5

instance Display SimplePolygon where
  render SimplePolygon { points } =
    Gloss.color (Gloss.greyN 0.7)
      . Gloss.polygon
      $ map (\Point { x, y } -> (100 * x, 100 * y)) points
