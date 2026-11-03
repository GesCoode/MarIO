module GameData where

import Graphics.Gloss (Picture)

-- | 2D point / vector. Object positions use the center.
data Vec2 = Vec2 {
    x :: Float, 
    y :: Float}
    deriving (Eq)

-- AABB: center + width + height.
data Hitbox = Hitbox {
    hbCenter :: Vec2, 
    hbWidth :: Float, 
    hbHeight :: Float}
    deriving (Eq)

-- Static level object (platform, block, …).
data StaticObj = StaticObj {
    soCenter :: Vec2, 
    soWidth :: Float, 
    soHeight :: Float, 
    soSprite :: String}
    deriving (Eq)

-- Include direction
data PlayerStart = PlayerStart {
    psCenter :: Vec2, 
    psVel :: Vec2}
    deriving (Eq)

data LevelId = Sandbox | Level1 | Level2 | Level3
  deriving (Eq)
  
levelName :: LevelId -> String
levelName Sandbox = "Sandbox"
levelName Level1  = "Level 1"
levelName Level2  = "Level 2"
levelName Level3  = "Level 3"

levelPath :: LevelId -> FilePath
levelPath Sandbox = "levels/sandbox.json"
levelPath Level1  = "levels/level1.json"
levelPath Level2  = "levels/level2.json"
levelPath Level3  = "levels/level3.json"

-- | Parsed level payload (level id comes from which file was loaded).
data LevelFile = LevelFile {
    lfStatics :: [StaticObj], 
    lfPlayers :: [PlayerStart]}
    deriving (Eq)

data Level = Level { 
    lvlId :: LevelId, 
    lvlStatics :: [StaticObj], 
    lvlPlayers :: [PlayerStart]}
    deriving (Eq)

data Player = Player { 
    playerPos :: Vec2, 
    playerVel :: Vec2, 
    playerW   :: Float, 
    playerH   :: Float,
    onGround  :: Bool,
    playerIn  :: InputHandler,
    facingRight :: Bool,    -- last walking direction
    walkPhase   :: Float}   -- walk cycle progress; frame = floor walkPhase `mod` 3
    deriving (Eq)

data GameState = GameState {
    gsLevel        :: Level, 
    gsPlayers      :: [Player],   -- index 0 = WASD, index 1 = arrows
    gsDebug        :: Bool}

data Screen = Menu | Playing GameState

type SpriteAtlas = [(String, Picture)]

data AppState = AppState {
    screen :: Screen, 
    sprites :: SpriteAtlas}

initialApp :: SpriteAtlas -> AppState
initialApp atlas = AppState Menu atlas

defaultPlayer :: Player
defaultPlayer = Player
    { playerPos = Vec2 0 0
    , playerVel = Vec2 0 0
    , playerW   = 32
    , playerH   = 42          -- sprites are 16x21, drawn at 2x
    , onGround  = False
    , playerIn  = noInput
    , facingRight = True
    , walkPhase   = 0
    }

spawnPlayer :: PlayerStart -> Player
spawnPlayer ps = defaultPlayer { playerPos = psCenter ps, playerVel = psVel ps }

mkGameState :: Level -> GameState
mkGameState lvl = GameState
    { gsLevel        = lvl
    , gsPlayers      = take 2 (starts ++ fallback)
    , gsDebug        = True
    }
  where
    starts   = map spawnPlayer (lvlPlayers lvl)
    -- if the level defines fewer than 2 spawns, place extra players next to the first
    base     = case starts of
                 (p:_) -> p
                 []    -> defaultPlayer
    fallback = [ base { playerPos = Vec2 (x (playerPos base) + 48 * fromIntegral i) (y (playerPos base)) }
               | i <- [length starts ..] :: [Int] ]

data InputHandler = InputHandler {
    inLeft  :: Bool,
    inRight :: Bool,
    inJump  :: Bool}
    deriving (Eq)

noInput :: InputHandler
noInput = InputHandler False False False

staticHitbox :: StaticObj -> Hitbox
staticHitbox s = Hitbox (soCenter s) (soWidth s) (soHeight s)

playerHitbox :: Player -> Hitbox
playerHitbox p = Hitbox (playerPos p) (playerW p) (playerH p)

overlaps :: Hitbox -> Hitbox -> Bool
overlaps a b =
  abs (x (hbCenter a) - x (hbCenter b)) < (hbWidth a + hbWidth b) / 2 &&
  abs (y (hbCenter a) - y (hbCenter b)) < (hbHeight a + hbHeight b) / 2
