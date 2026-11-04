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

-- Goomba spawn: center + walk direction (-1 = left, 1 = right).
data GoombaStart = GoombaStart {
    gsCenter :: Vec2,
    gsDir    :: Float}
    deriving (Eq)

-- Hammer Bro spawn: center only (he never moves).
newtype BroStart = BroStart { 
    bsCenter :: Vec2 }
    deriving (Eq)

data LevelId = Sandbox | Level1 | Level2 | Level3
  deriving (Eq)
  
levelName :: LevelId -> String
levelName Sandbox = "Sandbox"
levelName Level1  = "Level 1"
levelName Level2  = "Level 2"
levelName Level3  = "Level 3"

levelPath :: LevelId -> FilePath
levelPath Sandbox = "Levels/sandbox.json"
levelPath Level1  = "Levels/level1.json"
levelPath Level2  = "Levels/level2.json"
levelPath Level3  = "Levels/level3.json"

-- | Parsed level payload (level id comes from which file was loaded).
data LevelFile = LevelFile {
    lfStatics :: [StaticObj], 
    lfPlayers :: [PlayerStart],
    lfGoombas :: [GoombaStart],
    lfBros    :: [BroStart]}
    deriving (Eq)

data Level = Level { 
    lvlId :: LevelId, 
    lvlStatics :: [StaticObj], 
    lvlPlayers :: [PlayerStart],
    lvlGoombas :: [GoombaStart],
    lvlBros    :: [BroStart]}
    deriving (Eq)

-- | Controls for one player
data InputHandler = InputHandler {
    inLeft :: Bool,
    inRight :: Bool,
    inJump :: Bool}
    deriving (Eq)
   
noInput :: InputHandler
noInput = InputHandler False False False

data PlayerSelect = SinglePlayer | TwoPlayer
    deriving (Eq)

selectPlayer :: PlayerSelect -> Int
selectPlayer SinglePlayer = 1
selectPlayer TwoPlayer    = 2

data Player = Player { 
    playerPos :: Vec2, 
    playerVel :: Vec2, 
    playerW   :: Float, 
    playerH   :: Float,
    onGround  :: Bool,
    playerIn  :: InputHandler,
    playerDead  :: Bool,    -- dying: death sprite, no collisions, respawns below screen
    playerSpawn :: Vec2,
    facingRight :: Bool,    -- last walking direction
    walkPhase   :: Float}   -- walk cycle progress; frame = floor walkPhase `mod` 3
    deriving (Eq)
    
data GoombaState
    = Walking
    | Flat Float   -- squashed; seconds left before it flies off
    | Dying        -- flying off screen, no collisions
    deriving (Eq)

data Goomba = Goomba {
    gPos   :: Vec2,
    gVel   :: Vec2,
    gDir   :: Float,   -- -1 left, 1 right
    gState :: GoombaState,
    gPhase :: Float}   -- walk cycle progress
    deriving (Eq)

data HammerBro = HammerBro {
    broPos      :: Vec2,
    broCooldown :: Float,        -- seconds until he may start a throw
    broWindup   :: Maybe Float,  -- Just t: in throw pose (HT), t seconds left
    broAnim     :: Float,        -- time, drives the idle animation
    broDead     :: Bool,         -- stomped: upside down, falling off screen
    broVel      :: Vec2}         -- only used while dead
    deriving (Eq)

data Hammer = Hammer {
    hamPos  :: Vec2,
    hamVel  :: Vec2,
    hamAnim :: Float}   -- time, drives the spin animation
    deriving (Eq)
    
goombaW, goombaH :: Float
goombaW = 28   -- sprites are 14x15, drawn at 2x
goombaH = 30

broW, broH, hammerSize :: Float
broW       = 36   -- idle sprite is 24x32 drawn at 2x; hitbox a bit narrower
broH       = 60
hammerSize = 20   -- sprite is 16x16 drawn at 2x; hitbox smaller

spawnGoomba :: GoombaStart -> Goomba
spawnGoomba s = Goomba (gsCenter s) (Vec2 0 0) (signum (gsDir s)) Walking 0

spawnBro :: BroStart -> HammerBro
spawnBro s = HammerBro (bsCenter s) 0 Nothing 0 False (Vec2 0 0)

goombaHitbox :: Goomba -> Hitbox
goombaHitbox gb = Hitbox (gPos gb) goombaW goombaH

broHitbox :: HammerBro -> Hitbox
broHitbox b = Hitbox (broPos b) broW broH

hammerHitbox :: Hammer -> Hitbox
hammerHitbox h = Hitbox (hamPos h) hammerSize hammerSize

data Screen = Menu | Playing GameState

type SpriteAtlas = [(String, Picture)]

data AppState = AppState {
    screen       :: Screen, 
    sprites      :: SpriteAtlas,
    playerSelect :: PlayerSelect}

initialApp :: SpriteAtlas -> AppState
initialApp atlas = AppState Menu atlas SinglePlayer

data GameState = GameState {
    gsLevel        :: Level, 
    gsPlayerSelect :: PlayerSelect,
    gsPlayers      :: [Player],   -- index 0 = WASD, index 1 = arrows
    gsGoombas      :: [Goomba],
    gsBros         :: [HammerBro],
    gsHammers      :: [Hammer],
    gsScore        :: Int,
    gsPaused       :: Bool,
    gsRng          :: Int,        -- seed for random death arcs
    gsDebug        :: Bool}

defaultPlayer :: Player
defaultPlayer = Player { 
    playerPos = Vec2 0 0, 
    playerVel = Vec2 0 0, 
    playerW   = 32, 
    playerH   = 42,          -- sprites are 16x21, drawn at 2x
    onGround  = False,
    playerIn  = noInput,
    playerDead  = False, 
    playerSpawn = Vec2 0 0,
    facingRight = True,
    walkPhase   = 0}

spawnPlayer :: PlayerStart -> Player
spawnPlayer ps = defaultPlayer { 
    playerPos = psCenter ps, 
    playerVel = psVel ps, 
    playerSpawn = psCenter ps}

mkGameState :: PlayerSelect -> Level ->  Either String GameState
mkGameState select lvl
    | null startpos = Left "Level has no starting positions." 
    | otherwise = Right GameState {
        gsLevel        = lvl,
        gsPlayerSelect = select,
        gsPlayers      = take (selectPlayer select) (startpos ++ fallback),
        gsGoombas      = map spawnGoomba (lvlGoombas lvl),
        gsBros         = map spawnBro (lvlBros lvl),
        gsHammers      = [],
        gsScore        = 0,
        gsPaused       = False,
        gsRng          = 12345,
        gsDebug        = True}
        where
            startpos = map spawnPlayer (lvlPlayers lvl)
            -- if the level defines fewer than 2 spawns, place extra players next to the first
            base = head startpos 
            fallback = 
                [ base { playerPos = pos, playerSpawn = pos }
                | i <- [length startpos .. selectPlayer select - 1] :: [Int]
                , let pos = Vec2 (x (playerPos base) + 48 * fromIntegral i) (y (playerPos base)) ]

staticHitbox :: StaticObj -> Hitbox
staticHitbox object = Hitbox (soCenter object) (soWidth object) (soHeight object)

playerHitbox :: Player -> Hitbox
playerHitbox player = Hitbox (playerPos player) (playerW player) (playerH player)

-- | AABB-overlap: collision at edge does not count as overlap
overlaps :: Hitbox -> Hitbox -> Bool
overlaps a b =
  abs (x (hbCenter a) - x (hbCenter b)) < (hbWidth a + hbWidth b) / 2 &&
  abs (y (hbCenter a) - y (hbCenter b)) < (hbHeight a + hbHeight b) / 2