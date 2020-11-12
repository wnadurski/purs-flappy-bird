module System.Animation where

import Prelude
import Data.Component (Component(..), getAnimationRenderer, isAnimationRenderer)
import Data.Entity (hasComponent, mapComponentsWith)
import Data.Maybe (Maybe(..))
import Data.Scene (Scene, mapEntities)
import Data.Time.Duration (Seconds(..))

animationSystem :: Seconds -> Scene -> Scene
animationSystem delta =
  mapEntities (hasComponent isAnimationRenderer)
    $ mapComponentsWith getAnimationRenderer mapAnimationRenderer
  where
  mapAnimationRenderer d =
    AnimationRenderer
      $ case d.elapsed of
          Nothing -> d { elapsed = Just (Seconds 0.0) }
          Just timestamp -> d { elapsed = Just (timestamp <> delta) }
