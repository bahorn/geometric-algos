{-# LANGUAGE NamedFieldPuns #-}

module Display.ConvexHull where

import qualified Graphics.Gloss as Gloss

import ConvexHull.Common (Point (..), SimplePolygon (..))
import Display.Display

instance Display Point where
  render Point { x, y } = Gloss.translate (100 * x) (100 * y) $ Gloss.circleSolid 1.0

instance Display SimplePolygon where
  render SimplePolygon { points } = Gloss.polygon $ map (\Point { x, y } -> (100 * x, 100 * y)) points
