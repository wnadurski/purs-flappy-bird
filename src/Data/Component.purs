module Data.Component where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', Traversal', prism')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Predicate (Predicate(..))
import Data.String.Utils (startsWith)
import Data.Symbol (SProxy(..))
import Data.Vector2 (Point, Vector2, zeroVector)
import Graphics.Canvas (CanvasImageSource)
import Graphics.Drawing (Drawing)
import Prim.Row (class Union)
import Record (merge)

-- newtype Component = Component 
--     { name :: String,
--     }
type TransformProps
  = ( position :: Point
    , scale :: Vector2
    )

type TansformData
  = Record TransformProps

type KinematicsData
  = { v :: Vector2
    }

type ColliderData
  = { size :: Vector2,
      shift :: Vector2
    }

type EntityId
  = String

data Component
  = Transform TansformData
  | Size Vector2
  | Kinematics KinematicsData
  | DrawingRenderer Drawing
  | CanvasSpriteRenderer CanvasImageSource
  | Id EntityId
  | Collider ColliderData
  | RigidBody
  | Camera
  | Player

derive instance genericComponent :: Generic Component _

instance showComponent :: Show Component where
  show (DrawingRenderer _) = "DrawingRenderer(x)"
  show (CanvasSpriteRenderer _) = "CanvasSpriteRenderer(x)"
  show (Transform x) = "Transform(" <> show x <> ")"
  show (Size x) = "Size(" <> show x <> ")"
  show Camera = "Camera"
  show Player = "Player"
  show (Kinematics d) = "Kinematics(" <> show d <> ")"
  show (Id id) = "Id(" <> id <> ")"
  show (RigidBody) = "RigidBody"
  show (Collider d) = "Collider(" <> show d <> ")"

type ComponentFilter
  = Predicate Component

mkTransform :: Maybe Vector2 -> Maybe Vector2 -> Component
mkTransform pos scale = Transform ({ position: fromMaybe { x: 0.0, y: 0.0 } pos, scale: fromMaybe { x: 1.0, y: 1.0 } scale })

-- mkTransform2 :: forall attrs attrs_. Union attrs attrs_ TransformProps => Record attrs -> Component
-- mkTransform2 r = Transform (merge r {position: zeroVector, scale: zeroVector})
isTransform :: ComponentFilter
isTransform =
  Predicate case _ of
    Transform _ -> true
    _ -> false

checkTransform :: (Predicate TansformData) -> ComponentFilter
checkTransform (Predicate f) =
  Predicate case _ of
    Transform d -> f d
    _ -> false

getTransformData :: Component -> Maybe TansformData
getTransformData (Transform p) = Just p

getTransformData _ = Nothing

_position :: forall a r. Lens' { position :: a | r } a
_position = prop (SProxy :: SProxy "position")

_transform :: Prism' Component TansformData
_transform = prism' Transform getTransformData

_transformPosition :: Traversal' Component Point
_transformPosition = _transform <<< _position

isPlayer :: ComponentFilter
isPlayer =
  Predicate case _ of
    Player -> true
    _ -> false

isSize :: ComponentFilter
isSize =
  Predicate case _ of
    Size _ -> true
    _ -> false

getSize :: Component -> Maybe Vector2
getSize = case _ of
  Size x -> Just x
  _ -> Nothing

isId :: ComponentFilter
isId =
  Predicate case _ of
    Id _ -> true
    _ -> false

isIdEqual :: EntityId -> ComponentFilter
isIdEqual id =
  Predicate case _ of
    Id id' -> id' == id
    _ -> false

getId :: Component -> Maybe EntityId
getId = case _ of
  Id id -> Just id
  _ -> Nothing

isIdStartsWith :: String -> ComponentFilter
isIdStartsWith str =
  Predicate case _ of
    Id id -> str `startsWith` id
    _ -> false

isDrawingRenderer :: ComponentFilter
isDrawingRenderer = Predicate \c -> isJust $ getDrawingRendererData c

getDrawingRendererData :: Component -> Maybe Drawing
getDrawingRendererData (DrawingRenderer p) = Just p

getDrawingRendererData _ = Nothing

isCanvasSpriteRenderer :: ComponentFilter
isCanvasSpriteRenderer = Predicate \c -> isJust $ getCanvasSpriteRendererData c

getCanvasSpriteRendererData :: Component -> Maybe CanvasImageSource
getCanvasSpriteRendererData = case _ of
  CanvasSpriteRenderer d -> Just d
  _ -> Nothing

isCamera :: ComponentFilter
isCamera =
  Predicate case _ of
    Camera -> true
    _ -> false

getKinematics :: Component -> Maybe KinematicsData
getKinematics = case _ of
  Kinematics d -> Just d
  _ -> Nothing

isKinematics :: ComponentFilter
isKinematics = Predicate \c -> isJust $ getKinematics c

mkKinematics :: Maybe Vector2 -> Component
mkKinematics new = Kinematics { v: fromMaybe zeroVector new }

defaultKinematics :: Component
defaultKinematics = Kinematics { v: zeroVector }

_kinematics :: Prism' Component KinematicsData
_kinematics = prism' Kinematics getKinematics

_v :: forall a r. Lens' { v :: a | r } a
_v = prop (SProxy :: SProxy "v")

isRigidBody :: ComponentFilter
isRigidBody =
  Predicate case _ of
    RigidBody -> true
    _ -> false


getCollider :: Component -> Maybe ColliderData
getCollider = case _ of
  Collider d -> Just d
  _ -> Nothing

isCollider :: ComponentFilter
isCollider = Predicate \c -> isJust $ getCollider c

mkCollider :: Vector2 -> Vector2 -> Component
mkCollider size shift = Collider { size, shift }