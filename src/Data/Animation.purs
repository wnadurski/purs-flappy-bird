module Data.Animation where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, range)
import Data.GameGraphics.Canvas (imageSize)
import Data.Int (toNumber)
import Data.Vector2 (Vector2)
import FRP.Behavior.Mouse (position)
import Graphics.Canvas (CanvasImageSource)

type AnimationKey
  = String

data RepeatMode
  = Loop

data Frame
  = Frame
    { sheet :: CanvasImageSource
    , position :: Vector2
    , size :: Vector2
    }

getFrameData :: Frame -> { sheet :: CanvasImageSource, position :: Vector2, size :: Vector2 }
getFrameData (Frame d) = d

makeNFrames :: Int -> CanvasImageSource -> NonEmptyArray Frame
makeNFrames n sheet = range 0 (n - 1) <#> \index -> Frame { sheet, size: { x: frameWidth, y: frameHeight }, position: { x: frameWidth * (toNumber index), y: 0.0 } }
  where
  sheetSize = imageSize sheet

  frameWidth = sheetSize.w / (toNumber n)

  frameHeight = sheetSize.h

type Animation
  = { key :: String
    , frameRate :: Int
    , frames :: NonEmptyArray Frame
    , repeat :: RepeatMode
    }

mkAnimation :: String -> Int -> (NonEmptyArray Frame) -> RepeatMode -> Animation
mkAnimation key frameRate frames repeat = { key, frameRate, frames, repeat }
