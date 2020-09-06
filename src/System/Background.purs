module System.Background where

import Data.Scene (Scene)
import Data.Time.Duration (Seconds(..))

backgroundSystem :: Seconds -> Scene -> Scene
backgroundSystem d s = s