module Main (main) where

import SDL
import SDL.Image

import Data.Text (pack)
import Control.Monad (unless)

import Currymon


main :: IO ()
main = do
  initializeAll
  window <- createWindow (pack "Currymon") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let exiting = any eventIsExit events

  texture <- loadTexture renderer "./res/battle-concept1.png"
  clear renderer
  copy renderer texture Nothing (Just $ Rectangle (P $ V2 0 0) (V2 800 600))
  present renderer
  destroyTexture texture
  
  unless exiting (appLoop renderer)
