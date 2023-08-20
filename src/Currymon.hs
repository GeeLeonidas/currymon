module Currymon (
    gameRes
  , renderScale
  , gameWindowConfig
  , eventIsExit
  , textureBounds
  ) where

import SDL
import Foreign.C
import Control.Monad.IO.Class (MonadIO)


gameRes :: Integral a => V2 a
gameRes = V2 160 144

renderScale :: Integral a => a -> a
renderScale = (* 4)

gameWindowConfig :: WindowConfig
gameWindowConfig = WindowConfig {
    windowVisible         = True
  , windowResizable       = False
  , windowPosition        = Wherever
  , windowMode            = Windowed
  , windowInputGrabbed    = False
  , windowInitialSize     = renderScale <$> gameRes
  , windowHighDPI         = False
  , windowGraphicsContext = NoGraphicsContext
  , windowBorder          = True
  }

eventIsExit :: Event -> Bool
eventIsExit event =
  case eventPayload event of
    WindowClosedEvent _ -> True
    _ -> False

textureBounds :: MonadIO m => Texture -> m (V2 CInt)
textureBounds t = bounds <$> queryTexture t
  where bounds info = V2 (textureWidth info) (textureHeight info)