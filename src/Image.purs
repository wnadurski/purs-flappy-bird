module Image where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Canceler(..), Error, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as R
import Graphics.Canvas (CanvasImageSource, tryLoadImage)

loadImage :: String -> Aff CanvasImageSource
loadImage path =
  makeAff
    $ \cb -> do
        canceledRef <- R.new (Nothing)
        tryLoadImage path
          ( \mbSrc -> do
              canceledError <- R.read canceledRef
              case canceledError of
                Just _ -> pure unit
                Nothing -> case mbSrc of
                  Just src -> cb $ Right src
                  Nothing -> cb $ (Left (error $ "Could not load '" <> path <> "'"))
          )
        pure $ Canceler (\e -> liftEffect $ R.write (Just e) canceledRef)
