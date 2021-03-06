module System.Collision where

import Prelude
import Data.Array (filter, nubBy, nubByEq)
import Data.Collision (eqCollisions)
import Data.Component (_position, getCollider, getTransformData, isCollider, isId, isTransform)
import Data.Either (Either(..))
import Data.Entity (Entity, eqId, getComponent, hasComponent)
import Data.Lens (preview)
import Data.Maybe (fromMaybe)
import Data.Scene (filterEntities, Scene)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..), uncurry)
import Debug.Trace (spy)

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
          >>= (const $ if (t2.x + c2.shift.x + c2.size.x) < (t1.x + c1.shift.x) then Left false else Right true)
          >>= (const $ if (t2.x + c2.shift.x) > (t1.x + c1.shift.x) + c1.size.x then Left false else Right true)
          >>= (const $ if (t2.y + c2.shift.y + c2.size.y) < (t1.y + c1.shift.y) then Left false else Right true)
          >>= (const $ if (t2.y + c2.shift.y) > (t1.y + c1.shift.y + c1.size.y) then Left false else Right true)
    pure
      $ case result of
          Left _ -> false
          Right _ -> true

collisionSystem :: Seconds -> Scene -> Scene
collisionSystem delta scene =
  let
    collidingEntities = filterEntities scene (hasComponent isTransform && hasComponent isCollider && hasComponent isId)

    collisionChecks = Tuple <$> collidingEntities <*> collidingEntities

    collisions' = (nubByEq eqCollisions <<< filter (uncurry check) <<< filter (not <<< uncurry eqId)) collisionChecks
  in
    scene { collisions = collisions' }
