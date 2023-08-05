module Main (main) where

import SDL

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

  rendererDrawColor renderer $= V4 245 245 245 255
  clear renderer
  present renderer
  
  unless exiting (appLoop renderer)
