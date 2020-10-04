module Data.Entity where

import Prelude

import Data.Component (Component, ComponentFilter, EntityId, getId, isId)
import Data.Foldable (and, find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Predicate (Predicate(..))
import Data.Tuple (Tuple(..), uncurry)

newtype Entity
  = Entity (Array Component)

derive instance genericEnttiy :: Generic Entity _

instance showEntity :: Show Entity where
  show = genericShow

type EntityFilter
  = Predicate Entity

hasComponent :: ComponentFilter -> EntityFilter
hasComponent checker =
  Predicate \entity -> case getComponent checker entity of
    Just _ -> true
    Nothing -> false

askFilter :: EntityFilter -> Entity -> Boolean
askFilter (Predicate p) = p

hasComponents :: Array ComponentFilter -> EntityFilter
hasComponents filters = and (hasComponent <$> filters)

getComponent :: ComponentFilter -> Entity -> Maybe Component
getComponent checker (Entity components) = find (unwrap checker) components

_components :: forall a. Newtype Entity a => Iso' Entity a
_components = _Newtype

mapComponents :: ComponentFilter -> (Component -> Component) -> Entity -> Entity
mapComponents checker mapper (Entity components) =
  Entity $ components
    <#> (\c -> if (unwrap checker) c then mapper c else c)


mapComponentsWith :: forall a. (Component -> Maybe a) -> (a -> Component) -> Entity -> Entity
mapComponentsWith getData mapper (Entity components) =
  Entity $ components
    <#> maybe'' (map mapper <<< getData)


maybe'' :: forall a. (a -> Maybe a) -> a ->  a
maybe'' mapper default = fromMaybe default (mapper default)

getEntityId :: Entity -> Maybe EntityId
getEntityId e = getComponent isId e >>= getId 

eqId :: Entity -> Entity -> Boolean
eqId e1 e2 = fromMaybe false $ Tuple <$> getEntityId e1 <*> getEntityId e2 <#> (uncurry eq)