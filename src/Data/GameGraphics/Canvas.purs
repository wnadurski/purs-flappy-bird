module Data.GameGraphics.Canvas where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, Context2D)

foreign import imageSize :: CanvasImageSource -> { w :: Number, h :: Number }

foreign import setSmoothingEnabled :: Context2D -> Boolean -> Effect Unit