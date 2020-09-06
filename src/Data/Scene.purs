module Data.Scene where

import Prelude
import Data.Array (filter, head)
import Data.Entity (Entity, EntityFilter)
import Data.Maybe (Maybe)
import Data.Predicate (Predicate(..))

type Scene
  = { entities :: Array Entity
    }

filterEntities :: Scene -> EntityFilter -> Array Entity
filterEntities scene (Predicate filterF) = filter filterF scene.entities

mapEntities :: EntityFilter -> (Entity -> Entity) -> Scene -> Scene
mapEntities (Predicate f) mapper ({ entities }) =
  entities
    <#> ( \e ->
          if f e then
            mapper e
          else
            e
      )
    # \e -> { entities: e }

getEntity :: Scene -> EntityFilter -> Maybe Entity
getEntity s = head <<< filterEntities s
