module Data.Collision where

import Prelude
import Data.Entity (Entity, getEntityId)
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
