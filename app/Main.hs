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

  appLoop window renderer sprites fonts 0

  freeFonts fonts
  destroySprites sprites
  destroyRenderer renderer
  destroyWindow window
  SDL.Font.quit

appLoop :: Window -> Renderer -> HashMap String Texture -> HashMap String Font -> Int -> IO ()
appLoop window renderer sprites fonts count = do
  events <- pollEvents
  let exiting = any eventIsExit events

  let
    missingSprite = findWithDefault undefined "missing" sprites
    bg = findWithDefault missingSprite "battle-concept1" sprites
    font = findWithDefault undefined "PublicPixel" fonts
  tSur <- textBox font $ pack "Lorem ipsum, dolor sit amet"
  text <- createTextureFromSurface renderer tSur
  freeSurface tSur

  clear renderer
  draw bg (P $ V2 10 60)
  draw bg (P $ gameRes * V2 1 0 + V2 (-60) 10)
  draw text (P $ gameRes * V2 0 1 + V2 4 (-20))
  present renderer

  destroyTexture text

  threadDelay 30000
  unless exiting recur
  where
    textBox font = SDL.Font.blendedWrapped font (V4 245 245 245 0) (gameWidth-8)
    draw = drawTexture renderer renderScale
    recur = appLoop window renderer sprites fonts (count+1)
