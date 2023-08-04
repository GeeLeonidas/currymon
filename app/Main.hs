module Main (main) where

import SDL

import Linear (V4(..))
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
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 245 245 245 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)
