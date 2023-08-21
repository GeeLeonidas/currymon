module Main (main) where

import SDL
import SDL.Font

import Data.Text (pack)
import Control.Monad (unless)

import Currymon
import Control.Concurrent (threadDelay)
import Data.HashMap.Internal.Strict


main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize

  window <- createWindow (pack "Currymon") gameWindowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  sprites <- loadSprites renderer spritePaths
  fonts <- loadFonts fontPaths 8

  appLoop window renderer sprites fonts

  freeFonts fonts
  destroySprites sprites
  destroyRenderer renderer
  destroyWindow window
  SDL.Font.quit

appLoop :: Window -> Renderer -> HashMap String Texture -> HashMap String Font -> IO ()
appLoop window renderer sprites fonts = do
  events <- pollEvents
  let exiting = any eventIsExit events

  let
    missingSprite = findWithDefault undefined "missing" sprites
    bg = findWithDefault missingSprite "battle-concept1" sprites
    font = findWithDefault undefined "PublicPixel" fonts
  tSur <- SDL.Font.solid font (V4 255 0 0 0) (pack "DEBUG")
  text <- createTextureFromSurface renderer tSur
  freeSurface tSur

  let draw = drawTexture renderer renderScale
  clear renderer
  draw bg (P $ V2 0 0)
  draw text (P $ V2 0 0)
  present renderer

  destroyTexture text

  threadDelay 30000
  unless exiting (appLoop window renderer sprites fonts)
