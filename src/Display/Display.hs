module Display.Display (Display (..), toScreen) where

import Data.Maybe
import qualified Graphics.Gloss as Gloss

class Display a where
  render :: a -> Gloss.Picture

instance Display a => Display [a] where
  render = Gloss.pictures . map render

instance Display a => Display (Maybe a) where
  render = maybe Gloss.Blank render

toScreen :: Gloss.Picture -> IO ()
toScreen = Gloss.display
  (Gloss.InWindow "Geometric Algorithms" (512, 512) (10, 10))
  Gloss.white
