module Render where

import Prelude

import Color (rgba, white, rgb)
import Color.Scheme.Clrs (red)
import Color.Scheme.MaterialDesign (amber)
import Data.Component (Component(..), TansformData, getCanvasSpriteRendererData, getCollider, getDrawingRendererData, getSize, getTransformData, isCamera, isCanvasSpriteRenderer, isCollider, isDrawingRenderer, isSize, isTransform)
import Data.Entity (Entity(..), askFilter, getComponent, hasComponent, hasComponents)
import Data.Foldable (fold)
import Data.GameGraphics.Canvas (imageSize)
import Data.GameState (GameState)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Newtype (wrap)
import Data.Scene (Scene)
import Data.Scene (Status(..), filterEntities, getEntity)
import Data.Traversable (sequence, traverse)
import Data.Vector2 (Point, subtract, zeroVector)
import Effect (Effect, foreachE)
import Effect.Class.Console (log, logShow)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImage, drawImageScale)
import Graphics.Drawing (Drawing, Font, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, text)
import Graphics.Drawing as D
import Graphics.Drawing.Font (FontOptions, bold, font, light, monospace)
import Partial.Unsafe (unsafePartial)
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

processSprite :: Entity -> Entity -> Maybe { position :: Point, src :: CanvasImageSource, size :: Point }
processSprite camera e =
  let
    mbCameraPos = getComponent isTransform camera >>= getTransformData <#> _.position

    mbSprite = getComponent isCanvasSpriteRenderer e >>= getCanvasSpriteRendererData

    mbSpriteSize = (\size -> { x: size.w, y: size.h }) <<< imageSize <$> mbSprite

    mbPosition = getComponent isTransform e >>= getTransformData <#> _.position

    mbScale = getComponent isTransform e >>= getTransformData <#> _.scale

    doScale :: Point -> Point -> Point
    doScale size scale = { x: size.x * scale.x, y: size.y * scale.y }
  in
    sequenceRecord
      $ { position: subtract <$> mbPosition <*> mbCameraPos
        , size: doScale <$> mbSpriteSize <*> mbScale
        , src: mbSprite
        }

process :: Entity -> RenderEntity -> Maybe Render
process camera (DrawingRenderEntity e) = processDrawing camera e <#> DrawingRender

process camera (CanvasSpriteRenderEntity e) = processSprite camera e <#> CanvasSpriteRender

renderByType :: Context2D -> Render -> Effect Unit
renderByType ctx = case _ of
  DrawingRender drawing -> D.render ctx drawing
  CanvasSpriteRender d -> drawImageScale ctx d.src d.position.x d.position.y d.size.x d.size.y

data Render
  = DrawingRender Drawing
  | CanvasSpriteRender { position :: Point, src :: CanvasImageSource, size :: Point }

data RenderEntity
  = DrawingRenderEntity Entity
  | CanvasSpriteRenderEntity Entity

-- derive instance name :: Class Type
render :: Context2D -> GameState -> Effect Unit
render ctx state = do
  let
    debug = true
    newScene = if debug then addColliders state.scene else state.scene
    drawingEntities = filterEntities newScene $ (hasComponent isDrawingRenderer || hasComponent isCanvasSpriteRenderer) && hasComponent isTransform

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
                    CanvasSpriteRenderEntity e
              )
            <#> process camera
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
    pure unit
  pure unit


addColliders :: Scene -> Scene
addColliders scene = 
  let
    colliderEntities = filterEntities scene (hasComponent isCollider && hasComponent isTransform) 
    newEntities = colliderEntities 
      <#> ( \e -> 
        let 
          transform = unsafePartial $ fromJust $ getComponent isTransform e
          collider = unsafePartial $ fromJust $ getComponent isCollider e >>= getCollider
        in
          Entity [transform, DrawingRenderer $ outlined (outlineColor red <> lineWidth 2.0) (rectangle 0.0 0.0 collider.x collider.y)]
      )

  in
    scene {entities = scene.entities <> newEntities}


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

