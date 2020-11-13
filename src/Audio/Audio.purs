module Audio.Audio where

import Prelude
import Effect

foreign import data Audio :: Type

type AudioSettings
  = { url :: String
    , loop :: Boolean
    }

foreign import createAudio :: AudioSettings -> Audio

foreign import playAudio :: Audio -> Effect Unit
