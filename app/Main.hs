module Main (main) where

import SDL
import SDL.Font

import Data.Text (pack)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.HashMap.Internal.Strict
import System.Random

import Currymon


main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize

  window <- createWindow (pack "Currymon") gameWindowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  sprites <- loadSprites renderer spritePaths
  fonts <- loadFonts fontPaths 8
  tick <- ticks

  let rand = randoms $ mkStdGen $ fromIntegral tick
  appLoop window renderer sprites fonts initialBattleState rand 0

  freeFonts fonts
  destroySprites sprites
  destroyRenderer renderer
  destroyWindow window
  SDL.Font.quit

appLoop :: Window -> Renderer -> HashMap String Texture -> HashMap String Font -> BattleState -> [Int] -> Int -> IO ()
appLoop window renderer sprites fonts state rand count = do
  events <- pollEvents
  let
    exiting = any eventIsExit events
    newState@(BattleState fsm ally enemy items content messages) =
      updateBattleState state events rand count

  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  
  case fsm of
    MainBattle option -> draw $ mainBattleScene option
    _ -> pure ()

  present renderer

  threadDelay 30000
  unless exiting (appLoop window renderer sprites fonts newState (tail rand) (count + 1))
  where
    draw s = drawScene renderer renderScale s sprites fonts
