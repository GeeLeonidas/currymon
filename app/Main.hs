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

  clear renderer
  draw $ mainBattleScene "Lorem ipsum, dolor sit amet"
  present renderer

  threadDelay 30000
  unless exiting recur
  where
    draw s = drawScene renderer renderScale s sprites fonts
    recur = appLoop window renderer sprites fonts (count+1)
