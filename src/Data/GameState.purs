module Data.GameState where

import Prelude
import Data.Animation (Animation(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Scene (Scene)
import Data.Symbol (SProxy(..))

type GameState
  = { scene :: Scene
    , animations :: Array Animation
    }

_scene :: forall a r. Lens' { scene :: a | r } a
_scene = prop (SProxy :: SProxy "scene")

_entities :: forall a r. Lens' { entities :: a | r } a
_entities = prop (SProxy :: SProxy "entities")

_stateEntities :: forall a r r'. Lens' { scene :: { entities :: a | r' } | r } a
_stateEntities = _scene <<< _entities
