module Game where

import Prelude

import Color (white)
import Data.Animation (Animation(..), RepeatMode(..), makeNFrames, mkAnimation)
import Data.Collision (areColliding)
import Data.Component (Component(..), _kinematics, _v, defaultKinematics, isKinematics, isPlayer, mkAnimationRenderer, mkCollider, mkKinematics, mkTransform)
import Data.Entity (Entity(..), getEntityId, hasComponent, mapComponents)
import Data.Foldable (or)
import Data.GameGraphics.Canvas (imageSize, setSmoothingEnabled)
import Data.GameState (GameState, _scene)
import Data.Lens (over, set)
import Data.Maybe (Maybe(..))
import Data.Scene (Scene, Status(..), mapEntities)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Data.Vector2 (Vector2, add, zeroVector)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Enemy (enemiesSystem, initialEnemies)
import Graphics.Canvas (CanvasImageSource, Context2D, font)
import Graphics.Drawing (Drawing, Font, fillColor, text)
import Graphics.Drawing.Font (FontOptions, light, monospace)
import Image (loadImage)
import Random.LCG (mkSeed, unSeed)
import Record.Extra (sequenceRecord)
import Render (Render(..), render, uiFont)
import Resources (Resources)
import ScoreIndicator (scoreIndicatorSystem, scoreText)
import System.Animation (animationSystem)
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
    , mkAnimationRenderer "player-anim"
    , RigidBody
    , mkCollider { x: scale.x * (spriteSize.w / 4.0), y: scale.y * spriteSize.h } zeroVector
    ]
  where
  scale = { x: 5.0, y: 5.0 }

  spriteSize = imageSize res.player

initialState :: Int -> Resources -> Number -> Number -> GameState
initialState randomSeed resources w h =
  { scene:
      { status: Starting
      , randomSeed: unSeed newSeed
      , enemyIndex: 4
      , entities:
          [ Entity [ Camera, mkTransform (Just { x: 0.0, y: 0.0 }) (Nothing), mkKinematics (Just cameraVelocity), Size { x: w, y: h } ]
          , Entity [ Id "ground", mkTransform (Just { x: 0.0, y: h + 20.0 }) Nothing, mkCollider { x: w, y: 100.0 } zeroVector, mkKinematics (Just cameraVelocity) ]
          , Entity [ Id "ceiling", mkTransform (Just { x: 0.0, y: -180.0 }) Nothing, mkCollider { x: w, y: 100.0 } zeroVector, mkKinematics (Just cameraVelocity) ]
          ]
            <> backgroundEntities resources
            <> [ playerEntity resources cameraVelocity
              ]
            <> enemies
            <> [ Entity [ Id "scoreText", mkTransform (Just { x: 30.0, y: 30.0 }) Nothing, DrawingRenderer (scoreText ""), mkKinematics (Just cameraVelocity) ] ]
      , collisions: []
      }
  , animations: [ mkAnimation "player-anim" 8 (makeNFrames 4 resources.player) Loop, mkAnimation "enemy-anim" 5 (makeNFrames 6 resources.octopus) Loop ]
  }
  where
  cameraVelocity = { x: 200.0, y: 0.0 }

  (enemies /\ newSeed) = initialEnemies (mkSeed randomSeed) resources

loosingCollisions :: Array (Tuple String String)
loosingCollisions =
  [ "ground" /\ "player"
  , "octopus-1" /\ "player"
  , "octopus-2" /\ "player"
  , "octopus-3" /\ "player"
  , "ceiling" /\ "player"
  ]

update :: Seconds -> GameState -> Effect GameState
update delta state = do
  pure newState
  where
  pipeline =
    (if state.scene.status == Playing then collisionSystem delta else identity)
      <<< (if state.scene.status == Playing then physicsSystem delta else identity)
      <<< (if state.scene.status == Playing then backgroundSystem delta else identity)
      <<< (if state.scene.status == Playing then enemiesSystem delta else identity)
      <<< scoreIndicatorSystem delta
      <<< animationSystem delta

  newScene = pipeline state.scene

  areCollidingInScene = areColliding newScene.collisions

  newState = set _scene newScene state # (\newState -> if (or $ (uncurry areCollidingInScene) <$> loosingCollisions) then newState { scene { status = Loosing } } else newState)

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

game :: Resources -> Context2D -> Seconds -> GameState -> Aff GameState
game resources ctx delta state = do
  -- resources <- try $ sequenceRecord resources'
  liftEffect $ setSmoothingEnabled ctx false
  newState <- liftEffect $ update delta state
  liftEffect $ render ctx newState
  pure newState
