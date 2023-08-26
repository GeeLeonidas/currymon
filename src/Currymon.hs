module Currymon (
    gameWidth
  , gameHeight
  , gameRes
  , renderScale
  , gameWindowConfig
  , mainBattleScene
  , spritePaths
  , fontPaths
  , loadSprites
  , loadFonts
  , destroySprites
  , freeFonts
  , eventIsExit
  , textureBounds
  , drawTexture
  , drawScene
  ) where

import SDL
import SDL.Image
import SDL.Font

import Foreign.C
import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict
import System.FilePath
import Data.Text (pack)


gameWidth :: Integral a => a
gameWidth = 160

gameHeight :: Integral a => a
gameHeight = 144

gameRes :: Integral a => V2 a
gameRes = V2 gameWidth gameHeight

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

newtype Dice a = Dice [a]
instance Functor Dice where
  fmap f (Dice xs) = Dice $ f <$> xs

d1 :: Integral a => Dice a
d1 = Dice [1]

d4 :: Integral a => Dice a
d4 = Dice [1,2,3,4]

d6 :: Integral a => Dice a
d6 = Dice [1,2,3,4,5,6]

dCrazy :: Integral a => Dice a
dCrazy = Dice [0,0,21]

roll :: Dice a -> Int -> a
roll (Dice []) _ = error "No roll information for this Dice"
roll (Dice xs) i = xs !! (i `mod` length xs)

rollMultiple :: [Dice a] -> [Int] -> ([a], [Int])
rollMultiple []     rand = ([], rand)
rollMultiple (x:ys) rand = (roll x (head rand) : recurDamage, recurRand)
  where (recurDamage, recurRand) = rollMultiple ys (tail rand)

data MoveType = Rock | Paper | Scissors | Typeless
data Move = Move {
    moveName  :: String
  , moveDesc  :: String
  , moveType  :: MoveType
  , moveDices :: [Dice CInt]
  }

smack :: Move
smack = Move "Smack" "Rolls one d4 + 5 for damage" Rock [d4, (5*) <$> d1]

slit :: Move
slit = Move "Slit" "Rolls three d4's for damage" Paper [d4, d4, d4]

puncture :: Move
puncture = Move "Puncture" "Rolls two d6's for damage" Scissors [d6, d6]

crush :: Move
crush = Move "Crush" "Deals 21 damage, but misses 2/3 of the time" Typeless [dCrazy]

data Monster = Monster {
    monsterName     :: String
  , healthPoints    :: CInt
  , maxHealthPoints :: CInt
  , knownMoves      :: V4 Move
  }

lomba :: Monster
lomba = Monster "Lomba" maxHP maxHP (V4 smack slit puncture crush)
  where maxHP = 35

data Scene = Scene {
    spriteDraws :: [(String, Point V2 CInt)]
  , fontDraws   :: [(String, Color, Point V2 CInt, String)]
  }

mainBattleScene :: String -> Scene
mainBattleScene content = Scene sDraws fDraws
  where
    sDraws = [
        ("battle-concept1", P $ V2 10 60)
      , ("battle-concept1", P $ gameRes * V2 1 0 + V2 (-60) 10)
      ]
    fDraws = [
        ("PublicPixel", V4 245 245 245 255, P $ gameRes * V2 0 1 + V2 4 (-20), content)
      ]

data SceneFSM = MainBattle (V2 CInt) | MoveSelection (V2 CInt) | ItemSelection (V2 CInt)

advanceFSM :: SceneFSM -> [Event] -> SceneFSM
advanceFSM (MainBattle (V2 xIdx yIdx)) events
  | any eventIsActionConfirm events = if xIdx == 0 then MoveSelection (V2 0 0) else ItemSelection (V2 0 0)
  | otherwise = MainBattle $ V2 newXIdx newYIdx
  where
    xipa = xIdx + xAction
    yipa = yIdx + yAction
    newXIdx = min (max xipa 0) 1
    newYIdx = min (max yipa 0) 1
    xAction
      | any eventIsActionRight events =  1
      | any eventIsActionLeft events  = -1
      | otherwise                     =  0
    yAction
      | any eventisActionUp events   =  1
      | any eventIsActionDown events = -1
      | otherwise                    =  0

advanceFSM (MoveSelection (V2 xIdx yIdx)) events
  | any eventIsActionBack events = MainBattle (V2 0 0)
  | otherwise = MoveSelection $ V2 newXIdx newYIdx
  where
    xipa = xIdx + xAction
    yipa = yIdx + yAction
    newXIdx = min (max xipa 0) 1
    newYIdx = min (max yipa 0) 1
    xAction
      | any eventIsActionRight events =  1
      | any eventIsActionLeft events  = -1
      | otherwise                     =  0
    yAction
      | any eventisActionUp events   =  1
      | any eventIsActionDown events = -1
      | otherwise                    =  0

advanceFSM (ItemSelection (V2 _ idx)) events
  | any eventIsActionBack events = MainBattle (V2 1 0)
  | otherwise = ItemSelection $ V2 0 newIdx
  where
    ipa = idx + action
    newIdx = max ipa 0
    action
      | any eventisActionUp events   =  1
      | any eventIsActionDown events = -1
      | otherwise                    =  0

eventIsActionLeft :: Event -> Bool
eventIsActionLeft event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeLeft)
    _ -> False

eventIsActionRight :: Event -> Bool
eventIsActionRight event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeRight)
    _ -> False

eventisActionUp :: Event -> Bool
eventisActionUp event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeUp)
    _ -> False

eventIsActionDown :: Event -> Bool
eventIsActionDown event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeDown)
    _ -> False

eventIsActionConfirm :: Event -> Bool
eventIsActionConfirm event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeZ)
    _ -> False

eventIsActionBack :: Event -> Bool
eventIsActionBack event =
  case eventPayload event of
    KeyboardEvent k ->
      (keyboardEventKeyMotion k == Released) &&
      ((keysymScancode . keyboardEventKeysym) k == ScancodeX)
    _ -> False

eventIsExit :: Event -> Bool
eventIsExit event =
  case eventPayload event of
    WindowClosedEvent _ -> True
    _ -> False

spritePaths :: [FilePath]
spritePaths = [
    "./res/sprite/missing.png"
  , "./res/sprite/battle-concept1.png"
  ]

fontPaths :: [FilePath]
fontPaths = [
    "./res/font/public-pixel/PublicPixel.ttf"
  ]

loadSprites :: MonadIO m => Renderer -> [FilePath] -> m (HashMap String Texture)
loadSprites _ []     = pure empty
loadSprites r (x:ys) = union <$> (singleton key <$> loadTexture r x) <*> recur
  where
    key = takeBaseName x
    recur = loadSprites r ys

loadFonts :: MonadIO m => [FilePath] -> PointSize -> m (HashMap String Font)
loadFonts [] _     = pure empty
loadFonts (x:ys) n = union <$> (singleton key <$> SDL.Font.load x n) <*> recur
  where
    key = takeBaseName x
    recur = loadFonts ys n

destroySprites :: MonadIO m => HashMap String Texture -> m ()
destroySprites hm
  | hm == empty = pure ()
  | otherwise   = do
    destroyTexture texture
    recur
  where
    (key, texture) = head $ toList hm
    recur = destroySprites $ delete key hm

freeFonts :: MonadIO m => HashMap String Font -> m ()
freeFonts hm
  | hm == empty = pure ()
  | otherwise   = do
    SDL.Font.free font
    recur
  where
    (key, font) = head $ toList hm
    recur = freeFonts $ delete key hm

textureBounds :: MonadIO m => Texture -> m (V2 CInt)
textureBounds t = bounds <$> queryTexture t
  where bounds info = V2 (textureWidth info) (textureHeight info)

drawTexture :: MonadIO m => Renderer -> (CInt -> CInt) -> Texture -> Point V2 CInt -> m ()
drawTexture r f t p = do
  tb <- textureBounds t
  copy r t Nothing $ Just $ f <$> Rectangle p tb

drawScene :: MonadIO m => Renderer -> (CInt -> CInt) -> Scene -> HashMap String Texture -> HashMap String Font -> m ()
drawScene r f s hms hmf = do
  drawSprites r f s hms
  drawFonts r f s hmf
  where
    drawSprites :: MonadIO m => Renderer -> (CInt -> CInt) -> Scene -> HashMap String Texture -> m ()
    drawSprites _        _      (Scene []     _ ) _       = do pure ()
    drawSprites renderer factor (Scene (x:ys) zs) sprites = do
      drawSprites renderer factor (Scene ys zs) sprites
      let
        (key, pos) = x
        missingSprite = findWithDefault (error "\"missing\" sprite is missing") "missing" sprites
        currentSprite = findWithDefault missingSprite key sprites
      drawTexture renderer factor currentSprite pos
    drawFonts :: MonadIO m => Renderer -> (CInt -> CInt) -> Scene -> HashMap String Font -> m ()
    drawFonts _        _      (Scene _  []    ) _     = do pure ()
    drawFonts renderer factor (Scene zs (x:ys)) fonts = do
      drawFonts renderer factor (Scene zs ys) fonts
      let
        (key, color, pos@(P (V2 textX _)), content) = x
        font = findWithDefault (error $ '\"' : key ++ "\" font is missing") key fonts
      surface <- SDL.Font.blendedWrapped font color (gameWidth - 2 * fromIntegral textX) (pack content)
      texture <- createTextureFromSurface renderer surface
      drawTexture renderer factor texture pos
      freeSurface surface
      destroyTexture texture