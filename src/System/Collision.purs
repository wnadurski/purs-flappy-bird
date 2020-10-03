module System.Collision where

import Prelude
import Data.Component (_position, getCollider, getTransformData, isCollider, isTransform)
import Data.Either (Either(..))
import Data.Entity (Entity, getComponent, hasComponent)
import Data.Lens (preview)
import Data.Maybe (fromMaybe)
import Data.Scene (filterEntities, Scene)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..), uncurry)
import Data.Array (filter)

check :: Entity -> Entity -> Boolean
check e1 e2 =
  fromMaybe false do
    t1 <- getComponent isTransform e1 >>= getTransformData >>= (preview _position)
    c1 <- getComponent isCollider e1 >>= getCollider
    t2 <- getComponent isTransform e2 >>= getTransformData >>= (preview _position)
    c2 <- getComponent isCollider e2 >>= getCollider
    let
      result =
        Right true
          >>= (const $ if (t2.x + c2.x) < t1.x then Left false else Right true)
          >>= (const $ if t2.x > (t1.x + c1.x) then Left false else Right true)
          >>= (const $ if (t2.y + c2.y) < t1.y then Left false else Right true)
          >>= (const $ if t2.y > (t1.y + c1.y) then Left false else Right true)
    pure
      $ case result of
          Left _ -> false
          Right _ -> true

collisionSystem :: Seconds -> Scene -> Scene
collisionSystem delta scene =
  let
    collidingEntities = filterEntities scene (hasComponent isTransform && hasComponent isCollider)

    collisionChecks = Tuple <$> collidingEntities <*> collidingEntities

    collisions' = (filter (uncurry check) <<< filter (uncurry notEq)) collisionChecks
  in
    scene { collisions = collisions' }
