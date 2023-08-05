module Main (main) where

import SDL
import SDL.Image
import SDL.Font

import Data.Text (pack)
import Control.Monad (unless)

import Currymon


main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize
  window <- createWindow (pack "Currymon") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop window renderer
  destroyWindow window
  SDL.Font.quit

appLoop :: Window -> Renderer -> IO ()
appLoop window renderer = do
  events <- pollEvents
  let exiting = any eventIsExit events

  texture <- loadTexture renderer "./res/battle-concept1.png"
  font <- SDL.Font.load "./res/font/public-pixel/PublicPixel.ttf" 16
  surface <- SDL.Font.solid font (V4 255 0 0 0) (pack "DEBUG")
  text <- createTextureFromSurface renderer surface
  clear renderer
  copy renderer texture Nothing (Just $ Rectangle (P $ V2 0 0) (V2 800 600))
  copy renderer text Nothing (Just $ Rectangle (P $ V2 0 0) (V2 667 134))
  present renderer
  destroyTexture texture
  SDL.Font.free font
  freeSurface surface
  
  unless exiting (appLoop window renderer)
