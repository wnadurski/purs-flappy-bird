module ScoreIndicator where

import Prelude

import Color (white)
import Data.Component (Component(..), _position, getTransformData, isCamera, isDrawingRenderer, isIdEqual, isTransform)
import Data.Entity (getComponent, hasComponent, mapComponents)
import Data.Int (floor)
import Data.Lens (preview)
import Data.Maybe (fromMaybe)
import Data.Scene (Scene, getEntity, mapEntities)
import Data.Time.Duration (Seconds(..))
import Data.Vector2 (_x)
import Graphics.Drawing (Drawing, fillColor, text)
import Graphics.Drawing.Font (light)
import Render (uiFont)


calculateScore :: Number -> Int
calculateScore cameraXpos = floor $ cameraXpos / 100.0

scoreText :: String -> Drawing
scoreText = text (uiFont 20 light) 0.0 0.0 (fillColor white)

scoreIndicatorSystem :: Seconds -> Scene -> Scene
scoreIndicatorSystem secods scene =
  mapEntities (hasComponent $ isIdEqual "scoreText")
    (mapComponents isDrawingRenderer $ const $ DrawingRenderer $ scoreText (fromMaybe "" mbScore))
    scene
  where
    mbScore = getEntity scene (hasComponent isCamera) 
                >>= getComponent isTransform
                >>= getTransformData
                >>= preview (_position <<< _x)
                <#> calculateScore
                <#> show