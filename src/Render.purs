module Render where

import Prelude
import Color (rgba, white, rgb)
import Color.Scheme.Clrs (red)
import Color.Scheme.MaterialDesign (amber)
import Data.Animation (Animation(..), getFrameData)
import Data.Array.NonEmpty (length, (!!))
import Data.Component (Component(..), TansformData, getAnimationRenderer, getCanvasSpriteRendererData, getCollider, getDrawingRendererData, getSize, getTransformData, isAnimationRenderer, isCamera, isCanvasSpriteRenderer, isCollider, isDrawingRenderer, isSize, isTransform)
import Data.Entity (Entity(..), askFilter, getComponent, hasComponent, hasComponents)
import Data.Foldable (find, fold)
import Data.GameGraphics.Canvas (imageSize)
import Data.GameState (GameState)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Scene (Scene, Status(..))
import Data.Scene (Status(..), filterEntities, getEntity)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Data.Traversable (sequence, traverse)
import Data.Vector2 (Point, subtract, zeroVector)
import Effect (Effect, foreachE)
import Effect.Class.Console (log, logShow)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImage, drawImageFull, drawImageScale, restore, save, scale)
import Graphics.Drawing (Drawing, Font, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, text)
import Graphics.Drawing as D
import Graphics.Drawing.Font (FontOptions, bold, font, light, monospace)
import Partial.Unsafe (unsafePartial)
import Record as R
import Record.Extra (sequenceRecord)

translateDrawing :: Point -> Point -> Drawing -> Drawing
translateDrawing cameraPos drawingPos = D.translate (drawingPos.x - cameraPos.x) (drawingPos.y - cameraPos.y)

processDrawing :: Entity -> Entity -> Maybe Drawing
processDrawing camera e =
  let
    mbCameraPos = getComponent isTransform camera >>= getTransformData <#> _.position

    mbDrawing = getComponent isDrawingRenderer e >>= getDrawingRendererData

    mbPosition = getComponent isTransform e >>= getTransformData <#> _.position
  in
    translateDrawing <$> mbCameraPos <*> mbPosition <*> mbDrawing

processTransform :: Entity -> Entity -> Maybe { position :: Point, scale :: Point }
processTransform camera e =
  let
    mbCameraPos = getComponent isTransform camera >>= getTransformData <#> _.position

    mbPosition = getComponent isTransform e >>= getTransformData <#> _.position

    mbScale = getComponent isTransform e >>= getTransformData <#> _.scale
  in
    sequenceRecord
      $ { position: subtract <$> mbPosition <*> mbCameraPos
        , scale: mbScale
        }

processSprite :: Entity -> Entity -> Maybe { position :: Point, src :: CanvasImageSource, size :: Point, scale :: Point }
processSprite camera e =
  let
    mbSprite = getComponent isCanvasSpriteRenderer e >>= getCanvasSpriteRendererData

    mbSpriteSize = (\size -> { x: size.w, y: size.h }) <<< imageSize <$> mbSprite
  in
    R.merge
      <$> ( sequenceRecord
            $ { size: mbSpriteSize
              , src: mbSprite
              }
        )
      <*> (processTransform camera e)

processAnimation :: GameState -> Entity -> Entity -> Maybe { position :: Point, animation :: Animation, scale :: Point, elapsedTime :: Seconds }
processAnimation state camera e = R.merge <$> (processTransform camera e) <*> animationSpecific
  where
  mbAnimationData = getComponent isAnimationRenderer e >>= getAnimationRenderer

  animationSpecific =
    sequenceRecord
      { elapsedTime: mbAnimationData >>= R.get (SProxy :: SProxy "elapsed")
      , animation:
          do
            key <- mbAnimationData <#> R.get (SProxy :: SProxy "key")
            find (\anim -> anim.key == key) state.animations
      }

process :: GameState -> Entity -> RenderEntity -> Maybe Render
process _ camera (DrawingRenderEntity e) = processDrawing camera e <#> DrawingRender

process _ camera (CanvasSpriteRenderEntity e) = processSprite camera e <#> CanvasSpriteRender

process state camera (AnimationRenderEntity e) = processAnimation state camera e <#> AnimationRender

renderByType :: Context2D -> Render -> Effect Unit
renderByType ctx = case _ of
  DrawingRender drawing -> D.render ctx drawing
  CanvasSpriteRender d -> do
    save ctx
    scale ctx { scaleX: d.scale.x, scaleY: d.scale.y }
    drawImageScale ctx d.src ((d.position.x / d.scale.x) - (if d.scale.x < 0.0 then d.size.x else 0.0)) ((d.position.y / d.scale.y) - (if d.scale.y < 0.0 then d.size.y else 0.0)) d.size.x d.size.y
    restore ctx
  AnimationRender d -> do
    let
      frameIndex = (floor ((toNumber d.animation.frameRate) * (unwrap d.elapsedTime))) `mod` (length d.animation.frames)

      mbFrame = d.animation.frames !! frameIndex <#> getFrameData
    case mbFrame of
      Nothing -> pure unit
      Just frame -> do
        save ctx
        scale ctx { scaleX: d.scale.x, scaleY: d.scale.y }
        drawImageFull ctx frame.sheet (frame.position.x) (frame.position.y) (frame.size.x) (frame.size.y) ((d.position.x / d.scale.x) - (if d.scale.x < 0.0 then frame.size.x else 0.0)) ((d.position.y / d.scale.y) - (if d.scale.y < 0.0 then frame.size.y else 0.0)) frame.size.x frame.size.y
        restore ctx

data Render
  = DrawingRender Drawing
  | CanvasSpriteRender { position :: Point, src :: CanvasImageSource, size :: Point, scale :: Point }
  | AnimationRender { position :: Point, animation :: Animation, scale :: Point, elapsedTime :: Seconds }

data RenderEntity
  = DrawingRenderEntity Entity
  | CanvasSpriteRenderEntity Entity
  | AnimationRenderEntity Entity

-- derive instance name :: Class Type
render :: Context2D -> GameState -> Effect Unit
render ctx state = do
  let
    debug = false

    newScene = if debug then addColliders state.scene else state.scene

    drawingEntities = filterEntities newScene $ (hasComponent isDrawingRenderer || hasComponent isCanvasSpriteRenderer || hasComponent isAnimationRenderer) && hasComponent isTransform

    mcamera = getEntity state.scene (hasComponent isCamera)
  case mcamera of
    Nothing -> pure unit
    Just camera -> do
      let
        processedDrawingEntities =
          drawingEntities
            <#> ( \e ->
                  if askFilter (hasComponent isDrawingRenderer) e then
                    DrawingRenderEntity e
                  else
                    if askFilter (hasComponent isCanvasSpriteRenderer) e then
                      CanvasSpriteRenderEntity e
                    else
                      AnimationRenderEntity e
              )
            <#> process state camera
            # sequence
            # fold
      foreachE processedDrawingEntities (renderByType ctx)
      renderUI ctx state
      pure unit

uiFont :: Int -> FontOptions -> Font
uiFont = font monospace

renderUI :: Context2D -> GameState -> Effect Unit
renderUI ctx state = do
  let
    size = fromMaybe zeroVector $ getEntity state.scene (hasComponent isCamera) >>= getComponent isSize >>= getSize
  if state.scene.status /= Playing then
    D.render ctx (filled (fillColor $ rgba 0 0 0 0.5) $ rectangle 0.0 0.0 size.x size.y)
  else
    pure unit
  if state.scene.status == Starting then do
    D.render ctx $ text (uiFont 80 bold) 229.0 100.0 (fillColor white) "Chicken"
    D.render ctx $ text (uiFont 50 bold) 365.0 165.0 (fillColor white) "vs"
    D.render ctx $ text (uiFont 90 bold) 210.0 245.0 (fillColor (rgb 204 51 153)) "Octopus"
    D.render ctx $ text (uiFont 50 light) 230.0 500.0 (fillColor white) "PRESS SPACE"
    pure unit
  else
    if state.scene.status == Loosing then do
      D.render ctx $ text (uiFont 90 bold) 170.0 245.0 (fillColor (rgb 204 51 153)) "You died."
      D.render ctx $ text (uiFont 50 light) 230.0 500.0 (fillColor white) "PRESS SPACE"
    else
      pure unit
  pure unit

addColliders :: Scene -> Scene
addColliders scene =
  let
    colliderEntities = filterEntities scene (hasComponent isCollider && hasComponent isTransform)

    newEntities =
      colliderEntities
        <#> ( \e ->
              let
                transform = unsafePartial $ fromJust $ getComponent isTransform e

                collider = unsafePartial $ fromJust $ getComponent isCollider e >>= getCollider
              in
                Entity [ transform, DrawingRenderer $ outlined (outlineColor red <> lineWidth 2.0) (rectangle collider.shift.x collider.shift.y collider.size.x collider.size.y) ]
          )
  in
    scene { entities = scene.entities <> newEntities }

-- renderColliders :: Context2D -> GameState -> Effect Unit
-- renderColliders ctx state = do
--   let 
--     entities = filterEntities state.scene (hasComponent isCollider)
--     mcamera = getEntity state.scene (hasComponent isCamera)
--   case mcamera of
--     Nothing -> pure unit
--     Just camera -> do
--       let
--         processedDrawingEntities =
--           drawingEntities
--             <#> ( \e ->
--                   if askFilter (hasComponent isDrawingRenderer) e then
--                     DrawingRenderEntity e
--                   else
--                     CanvasSpriteRenderEntity e
--               )
--             <#> process camera
--             # sequence
--             # fold
--       foreachE processedDrawingEntities (renderByType ctx)
--       renderUI ctx state
--       pure unit
