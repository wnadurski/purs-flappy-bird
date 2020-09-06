module Data.Vector2 where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

type Point
  = { x :: Number
    , y :: Number
    }

type Vector2
  = Point

scalarProduct :: Number -> Vector2 -> Vector2
scalarProduct s v = { x: s * v.x, y: s * v.y }

add :: Vector2 -> Vector2 -> Vector2
add a b = { x: a.x + b.x, y: a.y + b.y }

subtract :: Vector2 -> Vector2 -> Vector2
subtract a b = add a (scalarProduct (-1.0) b)

_x :: forall a r. Lens' { x :: a | r } a
_x = prop (SProxy :: SProxy "x")

_y :: forall a r. Lens' { y :: a | r } a
_y = prop (SProxy :: SProxy "y")

zeroVector :: Vector2
zeroVector = { x: 0.0, y: 0.0 }

unitVector :: Vector2
unitVector = { x: 1.0, y: 1.0 }