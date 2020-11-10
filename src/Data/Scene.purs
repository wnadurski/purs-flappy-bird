module Data.Scene where

import Prelude

import Data.Array (filter, head)
import Data.Entity (Entity, EntityFilter)
import Data.Maybe (Maybe)
import Data.Predicate (Predicate(..))
import Data.Tuple (Tuple)
import Data.Collision

data Status = Starting | Playing | Pausing | Loosing

derive instance eqStatus :: Eq Status

type Scene
  = { entities :: Array Entity
    , status :: Status
    , collisions :: Array Collision
    , randomSeed :: Int
    , enemyIndex :: Int
    }

filterEntities :: Scene -> EntityFilter -> Array Entity
filterEntities scene (Predicate filterF) = filter filterF scene.entities

mapEntities :: EntityFilter -> (Entity -> Entity) -> Scene -> Scene
mapEntities (Predicate f) mapper scene =
  scene.entities
    <#> ( \e ->
          if f e then
            mapper e
          else
            e
      )
    # \e -> scene { entities = e }

getEntity :: Scene -> EntityFilter -> Maybe Entity
getEntity s = head <<< filterEntities s
