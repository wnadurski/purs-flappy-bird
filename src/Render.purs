module Render where

import Prelude
import Data.Component (Component(..), TansformData, getCanvasSpriteRendererData, getDrawingRendererData, getTransformData, isCamera, isCanvasSpriteRenderer, isDrawingRenderer, isTransform)
import Data.Entity (Entity, askFilter, getComponent, hasComponent, hasComponents)
import Data.Foldable (fold)
import Data.GameGraphics.Canvas (imageSize)
import Data.GameState (GameState)
import Data.Maybe (Maybe(..))
import Data.Scene (filterEntities, getEntity)
import Data.Traversable (sequence, traverse)
import Data.Vector2 (Point, subtract)
import Effect (Effect, foreachE)
import Effect.Class.Console (log, logShow)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImage, drawImageScale)
import Graphics.Drawing (Drawing)
import Graphics.Drawing as D
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
    drawingEntities = filterEntities state.scene $ (hasComponent isDrawingRenderer || hasComponent isCanvasSpriteRenderer) && hasComponent isTransform

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
      pure unit
