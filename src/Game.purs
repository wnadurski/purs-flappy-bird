module Game where

import Prelude
import Data.Collision (areColliding)
import Data.Component (Component(..), _kinematics, _v, defaultKinematics, isKinematics, isPlayer, mkCollider, mkKinematics, mkTransform)
import Data.Entity (Entity(..), getEntityId, hasComponent, mapComponents)
import Data.GameGraphics.Canvas (imageSize, setSmoothingEnabled)
import Data.GameState (GameState, _scene)
import Data.Lens (over, set)
import Data.Maybe (Maybe(..))
import Data.Scene (Scene, Status(..), mapEntities)
import Data.Time.Duration (Seconds)
import Data.Tuple.Nested ((/\))
import Data.Vector2 (Vector2, add)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Image (loadImage)
import Record.Extra (sequenceRecord)
import Render (Render(..), render)
import System.Background (backgroundSystem, backgroundTransform)
import System.Collision (collisionSystem)
import System.Physics (physicsSystem)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

backgroundEntities :: Resources -> Array Entity
backgroundEntities resources =
  [ Entity [ Id "bg-back-1", backgroundTransform 0, CanvasSpriteRenderer resources.backgroundBack, defaultKinematics ]
  , Entity [ Id "bg-back-2", backgroundTransform 1, CanvasSpriteRenderer resources.backgroundBack, defaultKinematics ]
  , Entity [ Id "bg-mid-1", backgroundTransform 0, CanvasSpriteRenderer resources.backgroundMiddle, defaultKinematics ]
  , Entity [ Id "bg-mid-2", backgroundTransform 1, CanvasSpriteRenderer resources.backgroundMiddle, defaultKinematics ]
  , Entity [ Id "bg-front-1", backgroundTransform 0, CanvasSpriteRenderer resources.backgroundFront, defaultKinematics ]
  , Entity [ Id "bg-front-2", backgroundTransform 1, CanvasSpriteRenderer resources.backgroundFront, defaultKinematics ]
  ]

playerEntity :: Resources -> Vector2 -> Entity
playerEntity res cameraVelocity =
  Entity
    [ Player
    , Id "player"
    , mkTransform (Just { x: 75.0, y: 50.0 }) (Just scale)
    , mkKinematics (Just cameraVelocity)
    , CanvasSpriteRenderer res.player
    , RigidBody
    , mkCollider { x: scale.x * spriteSize.w, y: scale.y * spriteSize.h }
    ]
  where
  scale = { x: 5.0, y: 5.0 }

  spriteSize = imageSize res.player

initialState :: Resources -> Number -> Number -> GameState
initialState resources w h =
  { scene:
      { status: Starting
      , entities:
          [ Entity [ Camera, mkTransform (Just { x: 0.0, y: 0.0 }) (Nothing), mkKinematics (Just cameraVelocity), Size { x: w, y: h } ]
          , Entity [ Id "ground", mkTransform (Just { x: 0.0, y: h + 20.0 }) Nothing, mkCollider { x: w, y: 100.0 }, mkKinematics (Just cameraVelocity) ]
          ]
            <> backgroundEntities resources
            <> [ playerEntity resources cameraVelocity
              ]
            <> [ Entity [ Id "octopus-1", mkTransform (Just { x: 450.0, y: 250.0 }) (Just ({ x: 4.0, y: 4.0 })), CanvasSpriteRenderer resources.octopus, mkCollider { x: 50.0, y: 100.0 } ] ]
      , collisions: []
      }
  }
  where
  cameraVelocity = { x: 200.0, y: 0.0 }

update :: Seconds -> GameState -> Effect GameState
update delta state = do
  pure newState
  where
  pipeline =
    (if state.scene.status == Playing then collisionSystem delta else identity)
      <<< (if state.scene.status == Playing then physicsSystem delta else identity)
      <<< (if state.scene.status == Playing then backgroundSystem delta else identity)

  newScene = pipeline state.scene

  newState = set _scene newScene state # (\newState -> if areColliding newScene.collisions "ground" "player" then newState { scene { status = Loosing } } else newState)

handlePlayerJump :: Scene -> Scene
handlePlayerJump =
  mapEntities
    (hasComponent isPlayer)
    ( mapComponents isKinematics
        $ over (_kinematics <<< _v) (add { x: 0.0, y: -1000.0 })
    )

handleSpace :: Scene -> Scene -> Scene
handleSpace initialScene scene = case scene.status of
  Starting -> scene { status = Playing }
  Playing -> handlePlayerJump scene
  Loosing -> initialScene { status = Playing }
  _ -> scene

eventHandlers :: Scene -> Array ({ type :: EventType, target :: Effect EventTarget, handler :: Event -> GameState -> Effect GameState })
eventHandlers initialScene =
  [ { type: EventType "keydown"
    , handler:
        \e state -> do
          let
            keyEvent = fromEvent e
          pure
            $ case keyEvent of
                Just e
                  | key e == " " -> over _scene (handleSpace initialScene) state
                _ -> state
    , target: toEventTarget <$> (document =<< window)
    }
  ]

type Resources
  = { backgroundBack :: CanvasImageSource
    , backgroundMiddle :: CanvasImageSource
    , backgroundFront :: CanvasImageSource
    , player :: CanvasImageSource
    , octopus :: CanvasImageSource
    }

loadResources :: Aff Resources
loadResources =
  sequenceRecord
    $ { backgroundBack: loadImage "resources/background/parallax-forest-back-trees.png"
      , backgroundMiddle: loadImage "resources/background/parallax-forest-middle-trees.png"
      , backgroundFront: loadImage "resources/background/parallax-forest-front-trees.png"
      , player: loadImage "resources/chicken.png"
      , octopus: loadImage "resources/octopus.png"
      }

game :: Resources -> Context2D -> Seconds -> GameState -> Aff GameState
game resources ctx delta state = do
  -- resources <- try $ sequenceRecord resources'
  liftEffect $ setSmoothingEnabled ctx false
  newState <- liftEffect $ update delta state
  liftEffect $ render ctx newState
  pure newState
