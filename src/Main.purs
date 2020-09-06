module Main where

import Data.DateTime.Instant (unInstant)
import Data.GameState (GameState)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(..), convertDuration)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Game (eventHandlers, game, initialState, loadResources)
import Graphics.Canvas (Context2D, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, unit, ($), (-), (<#>), (>>=))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)

init :: Number -> Number -> Context2D -> Window -> Aff Unit
init w h ctx win = do
  prevTimestampRef <- liftEffect $ Ref.new (Nothing :: Maybe Seconds)
  resources <- loadResources
  currentGameStateRef <- liftEffect $ Ref.new $ initialState resources w h
  liftEffect $ foreachE eventHandlers
    ( \desc -> do
        listener <-
          eventListener
            ( \e -> do
                currentState <- Ref.read currentGameStateRef
                newState <- desc.handler e currentState
                Ref.write newState currentGameStateRef
            )
        target <- desc.target
        addEventListener desc.type listener false target
    )
  let
    frame :: GameState -> Aff Unit
    frame state = do
      (timestamp :: Seconds) <- liftEffect $ now <#> unInstant <#> convertDuration
      prevTimestamp <- liftEffect $ Ref.read prevTimestampRef
      let
        delta = case prevTimestamp of
          Nothing -> Seconds 0.0
          Just prev -> Seconds $ (unwrap timestamp) - (unwrap prev)
      liftEffect $ Ref.write (Just timestamp) prevTimestampRef
      newState <- game resources ctx delta state
      liftEffect $ (Ref.write newState currentGameStateRef)
      _ <- liftEffect $ requestAnimationFrame (launchAff_ $ frame newState) win
      pure unit
  (liftEffect $ Ref.read currentGameStateRef) >>= frame
  pure unit

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let
    canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth canvas w
  _ <- setCanvasHeight canvas h
  win <- window
  launchAff_ $ init w h ctx win
  pure unit
