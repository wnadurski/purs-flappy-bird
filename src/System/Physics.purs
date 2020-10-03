module System.Physics where

import Prelude

import Data.Component (_kinematics, _transformPosition, _v, isKinematics, isRigidBody, isTransform)
import Data.Entity (Entity(..), getComponent, hasComponent, mapComponents)
import Data.Lens (over, preview)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Scene (Scene, mapEntities)
import Data.Time.Duration (Seconds)
import Data.Vector2 (Vector2, add, scalarProduct)

physicsSystem :: Seconds -> Scene -> Scene
physicsSystem delta = kinematics delta <<< gravity delta

kinematics :: Seconds -> Scene -> Scene
kinematics delta = 
  mapEntities
    (hasComponent isKinematics && hasComponent isTransform)
    mapEntity
  where
  deltaS = unwrap delta
  mapEntity entity =
    case mbVelocity of
        Nothing -> entity
        Just velocity ->
            mapComponents
            isTransform
            (\c -> over _transformPosition (add $ scalarProduct deltaS velocity) c)
            entity
    where
    mbVelocity = join $ preview (_kinematics <<< _v) <$> (getComponent isKinematics entity)

gravityAcc :: Vector2
gravityAcc = {x: 0.0, y: 1500.0}

gravity :: Seconds -> Scene -> Scene
gravity delta =
  mapEntities 
    (hasComponent isKinematics && hasComponent isRigidBody)
    (mapComponents isKinematics $ over (_kinematics <<< _v) (add $ scalarProduct deltaS gravityAcc))
  where
    deltaS = unwrap delta