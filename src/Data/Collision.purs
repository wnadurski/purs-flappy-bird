module Data.Collision where

import Prelude

import Data.Component (EntityId, getId, isId)
import Data.Entity (Entity, getComponent, getEntityId)
import Data.Foldable (any)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple, fst, snd)

type Collision
  = Tuple Entity Entity

eqCollisions :: Collision -> Collision -> Boolean
eqCollisions a b =
  fromMaybe false
    $ do
        idA1 <- getEntityId $ fst a
        idA2 <- getEntityId $ snd a
        idB1 <- getEntityId $ fst b
        idB2 <- getEntityId $ snd b
        pure $ (idA1 == idB1 && idA2 == idB2) || (idA1 == idB2 && idA2 == idB1)


isCollisionForEntities :: EntityId -> EntityId -> Collision -> Boolean
isCollisionForEntities id1 id2 col = fromMaybe false do
  colId1 <- getComponent isId (fst col) >>= getId
  colId2 <- getComponent isId (snd col) >>= getId
  pure $ (id1 == colId1 && id2 == colId2) || (id1 == colId2 && id2 == colId1)

areColliding :: Array Collision -> EntityId -> EntityId -> Boolean
areColliding collissions id1 id2 = any (isCollisionForEntities id1 id2) collissions