module System.Background where

import Prelude

import Data.Component (Component, _kinematics, _transformPosition, _v, checkTransform, getCanvasSpriteRendererData, getId, getSize, getTransformData, isCamera, isCanvasSpriteRenderer, isId, isIdEqual, isIdStartsWith, isKinematics, isSize, isTransform, mkKinematics, mkTransform)
import Data.Entity (askFilter, getComponent, hasComponent, mapComponents)
import Data.GameGraphics.Canvas (imageSize)
import Data.Int (toNumber)
import Data.Lens (over, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Predicate (Predicate(..))
import Data.Scene (Scene, getEntity, mapEntities)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Data.Vector2 (Vector2, scalarProduct)
import Graphics.Canvas (transform)
import Test.Entity (entity)

backgroundWidth :: Number
backgroundWidth = 272.0

backgroundScale :: Vector2
backgroundScale = { x: 3.8, y: 3.8 }

backgroundTransform :: Int ->  Component
backgroundTransform shift = mkTransform (Just { x: 0.0 + ((toNumber shift) * backgroundWidth * backgroundScale.x), y: 0.0 }) (Just backgroundScale)

backgroundSystem :: Seconds -> Scene -> Scene
backgroundSystem d = (backgroundSystemScrolling <<< backgroundSystemVelocity)

backgroundSystemScrolling :: Scene -> Scene
backgroundSystemScrolling s = mapEntities (hasComponent $ isIdStartsWith "bg-") mapBackground s
  where
  mbCameraX = do
    camera <- getEntity s (hasComponent isCamera)
    transform <- getComponent isTransform camera
    transformData <- getTransformData transform
    pure $ transformData.position.x

  mapBackground =
    let
      cameraX = fromMaybe 0.0 mbCameraX
    in
      mapComponents 
        (checkTransform (Predicate \t -> t.position.x <= cameraX - (backgroundWidth * backgroundScale.x)))
        (over _transformPosition $ const ({y: 0.0, x: cameraX + (backgroundWidth * backgroundScale.x)}))

backgroundSystemVelocity :: Scene -> Scene
backgroundSystemVelocity s = mapEntities (hasComponent $ isIdStartsWith "bg-") mapBackground s
  where
  cameraVelocity :: Maybe Vector2
  cameraVelocity = do
    camera <- getEntity s (hasComponent isCamera)
    kinematics <- getComponent isKinematics camera
    preview (_kinematics <<< _v) kinematics

  mapBackground entity =
    let
      factor = case entity of
        e
          | askFilter (hasComponent (isIdStartsWith "bg-back")) entity -> 0.75
        e
          | askFilter (hasComponent (isIdStartsWith "bg-mid")) entity -> 0.40
        e
          | askFilter (hasComponent (isIdStartsWith "bg-front")) entity -> 0.05
        _ -> 0.0
    in
      mapComponents isKinematics
        ( \kinematicsComponent ->
            mkKinematics $ (scalarProduct factor) <$> cameraVelocity
        )
        entity
