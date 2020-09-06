module Data.GameGraphics.Canvas where

import Graphics.Canvas (CanvasImageSource)

foreign import imageSize :: CanvasImageSource -> { w :: Number, h :: Number }
