module Main (main) where

import SDL
import SDL.Image
import SDL.Font

import Data.Text (pack)
import Control.Monad (unless)

import Currymon
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize
  window <- createWindow (pack "Currymon") gameWindowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop window renderer
  destroyWindow window
  SDL.Font.quit

appLoop :: Window -> Renderer -> IO ()
appLoop window renderer = do
  events <- pollEvents
  let exiting = any eventIsExit events

  -- TODO: Move these 'load'/'create' procedures outside the loop
  bg <- loadTexture renderer "./res/battle-concept1.png"
  font <- SDL.Font.load "./res/font/public-pixel/PublicPixel.ttf" 8
  tSur <- SDL.Font.solid font (V4 255 0 0 0) (pack "DEBUG")
  text <- createTextureFromSurface renderer tSur
  
  bgBounds <- textureBounds bg
  textBounds <- textureBounds text

  clear renderer
  copy renderer bg Nothing $ Just $ Rectangle (P $ V2 0 0) (renderScale <$> bgBounds)
  copy renderer text Nothing $ Just $ Rectangle (P $ V2 0 0) (renderScale <$> textBounds)
  present renderer

  -- TODO: Also move these 'free'/'destroy' procedures
  SDL.Font.free font
  freeSurface tSur
  destroyTexture bg
  destroyTexture text

  threadDelay 30000
  unless exiting (appLoop window renderer)
