module Currymon (
    gameWidth
  , gameHeight
  , gameRes
  , renderScale
  , gameWindowConfig
  , gameRendererConfig
  , mainBattleScene
  , moveSelectionScene
  , itemSelectionScene
  , battleDialogScene
  , Monster(..)
  , SceneFSM(..)
  , BattleState(..)
  , initialBattleState
  , updateBattleState
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
import qualified Debug.Trace as Debug


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

gameRendererConfig :: [RendererInfo] -> RendererConfig
gameRendererConfig driverInfo
  | all (== SoftwareRenderer) rendererTypes =
    RendererConfig SoftwareRenderer False
  | AcceleratedVSyncRenderer `elem` rendererTypes =
    RendererConfig AcceleratedVSyncRenderer False
  | AcceleratedRenderer `elem` rendererTypes =
    RendererConfig AcceleratedRenderer False
  | otherwise =
    RendererConfig UnacceleratedRenderer False
  where rendererTypes = rendererType . rendererInfoFlags <$> driverInfo

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
  deriving Eq
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

hasAdvantageOver :: Move -> Move -> Bool
hasAdvantageOver (Move _ _ allyType _) (Move _ _ enemyType _) = case enemyType of
  Typeless -> allyType /= Typeless
  Scissors -> allyType == Rock
  Paper    -> allyType == Scissors
  Rock     -> allyType == Paper

data Monster = Monster {
    monsterName     :: String
  , healthPoints    :: CInt
  , maxHealthPoints :: CInt
  , knownMoves      :: V4 Move
  , buffedTurns     :: Int
  }

lomba :: Monster
lomba = Monster "Lomba" maxHP maxHP (V4 smack slit puncture crush) 0
  where maxHP = 35

selectMove :: Int -> Monster -> Move
selectMove i (Monster _ _ _ moves _) =
  case i `mod` 4 of
    0 -> let (V4 r _ _ _) = moves in r
    1 -> let (V4 _ r _ _) = moves in r
    2 -> let (V4 _ _ r _) = moves in r
    3 -> let (V4 _ _ _ r) = moves in r
    _ -> undefined

receiveMultipleDamage :: Monster -> [CInt] -> Monster
receiveMultipleDamage (Monster n hp mhp kms bt) ds = Monster n (max newHP 0) mhp kms bt
  where newHP = hp - sum ds

decreaseBuff :: Monster -> Monster
decreaseBuff (Monster n hp mhp kms bt) = Monster n hp mhp kms $ max (bt - 1) 0

useMove :: Int -> Monster -> Monster -> [Int] -> (Monster, Monster, [String], [Int])
useMove idx ally enemy rand = (decreaseBuff finalAlly, decreaseBuff finalEnemy, messages, finalRand)
  where
    allyMove = selectMove idx ally
    enemyMove = selectMove (head rand) enemy
    (allyDamagesWoBuff, newRand) = rollMultiple (moveDices allyMove) (tail rand)
    (enemyDamagesWoBuff, finalRand) = rollMultiple (moveDices enemyMove) newRand
    allyMoveBuff = ((if allyMove `hasAdvantageOver` enemyMove then 2 else 1)*)
    enemyMoveBuff = ((if enemyMove `hasAdvantageOver` allyMove then 2 else 1)*)
    allyBuff = allyMoveBuff . ((if buffedTurns ally > 0 then 2 else 1)*)
    enemyBuff = enemyMoveBuff . ((if buffedTurns enemy > 0 then 2 else 1)*)
    allyDamages = allyBuff <$> allyDamagesWoBuff
    enemyDamages = enemyBuff <$> enemyDamagesWoBuff
    diff = length allyDamages - length enemyDamages
    (newAlly, newEnemy)
      | diff > 0  = (ally, receiveMultipleDamage enemy (take diff allyDamages))
      | diff < 0  = (receiveMultipleDamage ally (take (-diff) enemyDamages), enemy)
      | otherwise = (ally, enemy)
    (newAllyDamages, newEnemyDamages)
      | diff > 0  = (drop diff allyDamages, enemyDamages)
      | diff < 0  = (allyDamages, drop (-diff) enemyDamages)
      | otherwise = (allyDamages, enemyDamages)
    (finalAlly, finalEnemy)
      | healthPoints newAlly  <= 0 = (newAlly, newEnemy)
      | healthPoints newEnemy <= 0 = (newAlly, newEnemy)
      | otherwise = (
          receiveMultipleDamage newAlly newEnemyDamages
        , receiveMultipleDamage newEnemy newAllyDamages
        )
    messages = (if diff >= 0 then id else reverse) [
        "Ally " ++ monsterName ally ++ if healthPoints newAlly > 0
          then
            " rolled " ++ if healthPoints newEnemy > 0
              then show allyDamages ++ "!"
              else show (take diff allyDamages) ++ " first!"
          else " fainted!"
      , "Enemy " ++ monsterName enemy ++ if healthPoints newEnemy > 0
          then
            " rolled " ++ if healthPoints newAlly > 0
              then show enemyDamages ++ "!"
              else show (take (-diff) enemyDamages) ++ " first!"
          else " fainted!"
      ] ++ ["Ally " ++ monsterName ally ++ " fainted!" | healthPoints newAlly > 0 && healthPoints finalAlly <= 0]
       ++ ["Enemy " ++ monsterName enemy ++ " fainted!" | healthPoints newEnemy > 0 && healthPoints finalEnemy <= 0]

data ItemType = Potion | Buff
  deriving Eq

data Item = Item {
    itemName  :: String
  , itemtype  :: ItemType
  }

useItem :: Item -> Monster -> Monster -> (Monster, Monster)
useItem item ally enemy | itemtype item == Potion = (healedAlly, enemy)
  where
    healedAlly = ally { healthPoints = min (healthPoints ally + 15) (maxHealthPoints ally) }

data Scene = Scene {
    spriteDraws :: [(String, Point V2 CInt)]
  , fontDraws   :: [(String, Color, Point V2 CInt, String, Bool)]
  }

mainBattleScene :: Integral a => V2 a -> Scene
mainBattleScene sel = Scene sDraws fDraws
  where
    fightContent = (if sel == V2 0 0 then ">" else "") ++ "Fight"
    itemContent = (if sel == V2 1 0 then ">" else "") ++ "Item"
    monsterContent = (if sel == V2 0 1 then ">" else "") ++ "Monster"
    runContent = (if sel == V2 1 1 then ">" else "") ++ "Run"
    sDraws = [
        ("battle-concept1", P $ V2 10 60)
      , ("battle-concept1", P $ gameRes * V2 1 0 + V2 (-60) 10)
      ]
    fDraws = [
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 0 1 + V2 8 (-28), fightContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 1 1 - V2 74 28, itemContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 0 1 + V2 8 (-16), monsterContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 1 1 - V2 74 16, runContent, False)
      ]

moveSelectionScene :: Integral a => V2 a -> Monster -> Scene
moveSelectionScene sel ally = Scene sDraws fDraws
  where
    moveOneContent = (if sel == V2 0 0 then ">" else "") ++ moveName (selectMove 0 ally)
    moveTwoContent = (if sel == V2 1 0 then ">" else "") ++ moveName (selectMove 1 ally)
    moveThreeContent = (if sel == V2 0 1 then ">" else "") ++ moveName (selectMove 2 ally)
    moveFourContent = (if sel == V2 1 1 then ">" else "") ++ moveName (selectMove 3 ally)
    sDraws = [
        ("battle-concept1", P $ V2 10 60)
      , ("battle-concept1", P $ gameRes * V2 1 0 + V2 (-60) 10)
      ]
    fDraws = [
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 0 1 + V2 8 (-28), moveOneContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 1 1 - V2 74 28, moveTwoContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 0 1 + V2 8 (-16), moveThreeContent, False),
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 1 1 - V2 74 16, moveFourContent, False)
      ]

-- TODO
itemSelectionScene :: Integral a => V2 a -> [Item] -> Scene
itemSelectionScene (V2 _ sel) items = Scene sDraws fDraws
  where
    itemCount = fromIntegral $ length items
    content = [(if (sel `mod` itemCount) == i then ">" else "") ++ itemName item | (item, i) <- zip items [0..]]
    sDraws = [
        ("battle-concept1", P $ V2 10 60)
      , ("battle-concept1", P $ gameRes * V2 1 0 + V2 (-60) 10)
      ]
    fDraws = [
        ("PublicPixel", V4 0 0 0 255, P $ V2 8 (28 + 12 * i), curContent, False)
        | (curContent, i) <- zip content [0..]
      ]

-- TODO
battleDialogScene :: CInt -> String -> Scene
battleDialogScene allyHP content = Scene sDraws fDraws
  where
    sDraws = [
        ("battle-concept1", P $ V2 10 60)
      , ("battle-concept1", P $ gameRes * V2 1 0 + V2 (-60) 10)
      ]
    fDraws = [
        ("PublicPixel", V4 0 0 0 255, P $ gameRes * V2 0 1 + V2 8 (-28), content, True)
      ]

data SceneFSM = MainBattle (V2 CInt) | MoveSelection (V2 CInt) | ItemSelection (V2 CInt) | BattleDialog

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
      | any eventIsActionDown events =  1
      | any eventIsActionUp events   = -1
      | otherwise                    =  0

advanceFSM (MoveSelection (V2 xIdx yIdx)) events
  | any eventIsActionConfirm events = BattleDialog
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
      | any eventIsActionDown events =  1
      | any eventIsActionUp events   = -1
      | otherwise                    =  0

advanceFSM (ItemSelection (V2 _ idx)) events
  | any eventIsActionConfirm events = BattleDialog
  | any eventIsActionBack events = MainBattle (V2 1 0)
  | otherwise = ItemSelection $ V2 0 newIdx
  where
    ipa = idx + action
    newIdx = max ipa 0
    action
      | any eventIsActionUp events   =  1
      | any eventIsActionDown events = -1
      | otherwise                    =  0

advanceFSM BattleDialog _ = BattleDialog

data BattleState = BattleState {
    sceneFSM       :: SceneFSM
  , allyMonster    :: Monster
  , enemyMonster   :: Monster
  , availableItems :: [Item]
  , dialogContent  :: String
  , dialogMessages :: [String]
  }

initialBattleState :: BattleState
initialBattleState = BattleState (MainBattle $ V2 0 0) lomba lomba [Item "Poçao" Potion, Item "Ataque" Buff] "" []

updateBattleState :: BattleState -> [Event] -> [Int] -> Int -> (BattleState, [Int])
updateBattleState (BattleState fsm@(MoveSelection (V2 xIdx yIdx)) ally enemy items content messages) events rand _ =
  (newBattleState, newRand)
  where
    newBattleState = case newFSM of
      BattleDialog -> BattleState newFSM newAlly newEnemy items "" newMessages
      _ -> BattleState newFSM ally enemy items content messages
    newFSM = advanceFSM fsm events
    idx = fromIntegral $ xIdx + yIdx
    (newAlly, newEnemy, newMessages, newRand) = useMove idx ally enemy rand

-- TODO: Create `data Item`
updateBattleState (BattleState (ItemSelection (V2 _ idx)) ally enemy items content messages) events rand count = undefined

updateBattleState (BattleState BattleDialog ally enemy items content (x:ys)) events rand count
  | any eventIsActionConfirm events = if content == x
    then ini "" ys
    else ini x (x:ys)
  | otherwise = if content /= x && even count
    then ini (take (length content + 1) x) (x:ys)
    else ini content (x:ys)
  where ini c m = (BattleState BattleDialog ally enemy items c m, rand)

updateBattleState (BattleState BattleDialog ally enemy items _ []) _ rand _ =
  (BattleState (MainBattle $ V2 0 0) ally enemy items "" [], rand)

updateBattleState (BattleState fsm ally enemy items content messages) events rand _ =
  (BattleState (advanceFSM fsm events) ally enemy items content messages, rand)

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

eventIsActionUp :: Event -> Bool
eventIsActionUp event =
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
        (key, color, pos@(P (V2 textX _)), rawContent, shouldWrap) = x
        content = if Prelude.null rawContent then " " else rawContent 
        font = findWithDefault (error $ '\"' : key ++ "\" font is missing") key fonts
      surface <- if shouldWrap
        then SDL.Font.blendedWrapped font color (gameWidth - 2 * fromIntegral textX) (pack content)
        else SDL.Font.solid font color (pack content)
      texture <- createTextureFromSurface renderer surface
      drawTexture renderer factor texture pos
      freeSurface surface
      destroyTexture texture