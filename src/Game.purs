module Game where

import Prelude
import Color.Scheme.MaterialDesign (amber, blueGrey)
import Data.Component (Component(..), _transformPosition, isPlayer, isTransform, mkKinematics, mkTransform)
import Data.Entity (Entity(..), hasComponent, mapComponents)
import Data.GameState (GameState, _scene, _stateEntities)
import Data.Identity (Identity(..))
import Data.Lens (over, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Scene (mapEntities)
import Data.Time.Duration (Seconds)
import Data.Vector2 (_x)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, runAff, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Drawing (fillColor, filled, rectangle)
import Image (loadImage)
import Record.Extra (sequenceRecord)
import Render (render)
import System.Physics (physicsSystem)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)

backgroundTransform :: Component
backgroundTransform = mkTransform (Just { x: 0.0, y: 0.0 }) (Just { x: 3.8, y: 3.8 })

initialState :: Resources -> Number -> Number -> GameState
initialState resources w h =
  { scene:
      { entities:
          [ Entity [ Camera, mkTransform (Just { x: 0.0, y: 0.0 }) (Nothing), mkKinematics (Just { x: 100.0, y: 0.0 }), Size { width: w, height: h } ]
          , Entity [ backgroundTransform, CanvasSpriteRenderer resources.backgroundBack ]
          , Entity [ backgroundTransform, CanvasSpriteRenderer resources.backgroundMiddle ]
          , Entity [ backgroundTransform, CanvasSpriteRenderer resources.backgroundFront ]
          -- , Entity [ Player, Transform { position: { x: 100.0, y: 20.0 } }, DrawingRenderer (filled (fillColor amber) $ (rectangle 0.0 0.0 100.0 100.0)) ]
          ]
      }
  }

update :: Seconds -> GameState -> Effect GameState
update delta state = pure newState
  where
  velocity = 122.0

  newScene = physicsSystem delta state.scene

  newState = set _scene newScene state

eventHandlers :: Array ({ type :: EventType, target :: Effect EventTarget, handler :: Event -> GameState -> Effect GameState })
eventHandlers =
  [ { type: EventType "keydown"
    , handler:
        \e state -> do
          log "Handler"
          pure state
    , target: toEventTarget <$> (document =<< window)
    }
  ]

type Resources
  = { backgroundBack :: CanvasImageSource
    , backgroundMiddle :: CanvasImageSource
    , backgroundFront :: CanvasImageSource
    }

loadResources :: Aff Resources
loadResources =
  sequenceRecord
    $ { backgroundBack: loadImage "/resources/background/parallax-forest-back-trees.png"
      , backgroundMiddle: loadImage "/resources/background/parallax-forest-middle-trees.png"
      , backgroundFront: loadImage "/resources/background/parallax-forest-front-trees.png"
      }

game :: Resources -> Context2D -> Seconds -> GameState -> Aff GameState
game resources ctx delta state = do
  -- resources <- try $ sequenceRecord resources'
  newState <- liftEffect $ update delta state
  liftEffect $ render ctx newState
  pure newState
