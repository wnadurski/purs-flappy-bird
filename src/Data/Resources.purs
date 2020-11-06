module Resources where

import Prelude

import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource)
import Image (loadImage)
import Record.Extra (sequenceRecord)

type Resources
  = { backgroundBack :: CanvasImageSource
    , backgroundMiddle :: CanvasImageSource
    , backgroundFront :: CanvasImageSource
    , player :: CanvasImageSource
    , octopus :: CanvasImageSource
    }

loadResources :: Aff Resources
loadResources =
  sequenceRecord
    $ { backgroundBack: loadImage "resources/background/parallax-forest-back-trees.png"
      , backgroundMiddle: loadImage "resources/background/parallax-forest-middle-trees.png"
      , backgroundFront: loadImage "resources/background/parallax-forest-front-trees.png"
      , player: loadImage "resources/chicken.png"
      , octopus: loadImage "resources/octopus.png"
      }