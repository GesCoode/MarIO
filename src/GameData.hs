module GameData where

import Graphics.Gloss (Picture)

-- Why not just use a library
data Vec2 = Vec2 {
    x :: Float, 
    y :: Float}
    deriving (Eq)

-- center + width + height.
data Hitbox = Hitbox {
    hbCenter :: Vec2, 
    hbWidth :: Float, 
    hbHeight :: Float}
    deriving (Eq)

-- Static level object
data StaticObj = StaticObj {
    soCenter :: Vec2, 
    soWidth :: Float, 
    soHeight :: Float, 
    soSprite :: String}
    deriving (Eq)

data PlayerStart = PlayerStart {
    psCenter :: Vec2, 
    psVel :: Vec2}
    deriving (Eq)

-- Goomba spawn
data GoombaStart = GoombaStart {
    gsCenter :: Vec2,
    gsDir    :: Float}
    deriving (Eq)

-- Hammer Bro spawn
newtype BroStart = BroStart { 
    bsCenter :: Vec2 }
    deriving (Eq)

data LevelId = Sandbox | Level1 | Level3 | RandomVertical
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
  
levelName :: LevelId -> String
levelName Sandbox = "Sandbox"
levelName Level1  = "World 1-1"
levelName Level3  = "Boss"
levelName RandomVertical = "Random"

levelPath :: LevelId -> Maybe FilePath
levelPath Sandbox = Just "Levels/sandbox.json"
levelPath Level1  = Just "Levels/level1.json"
levelPath Level3  = Just "Levels/level3.json"
levelPath RandomVertical = Nothing

data SwitchType = ToVertical | ToHorizontal LevelId | ToEnd
    deriving (Eq)

data Switch = Switch {
    switchPos :: Vec2,
    switchType :: SwitchType}
    deriving (Eq)
    
-- Parsed level
data LevelFile = LevelFile {
    lfStatics :: [StaticObj], 
    lfPlayers :: [PlayerStart],
    lfGoombas :: [GoombaStart],
    lfBros    :: [BroStart],
    lfGoals   :: [Vec2],
    lfBosses  :: [Vec2],
    lfSwitches  :: [Switch]}
    deriving (Eq)

data Level = Level { 
    lvlId       :: LevelId, 
    lvlStatics  :: [StaticObj], 
    lvlPlayers  :: [PlayerStart],
    lvlGoombas  :: [GoombaStart],
    lvlBros     :: [BroStart],
    lvlGoals    :: [Vec2],
    lvlBosses   :: [Vec2],
    lvlSwitches :: [Switch],
    lvlPlatforms :: [StaticObj]}
    deriving (Eq)




-- Controls for player
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

data GameMode = LevelSelect | Campaign
    deriving (Eq)

data GameStatus = InProgress | Win | Loss
    deriving (Eq)
    
data Player = Player { 
    playerPos :: Vec2, 
    playerVel :: Vec2, 
    playerW   :: Float, 
    playerH   :: Float,
    onGround  :: Bool,
    playerIn  :: InputHandler,
    playerDead  :: Bool,
    playerHealth :: Int,
    playerSpawn :: Vec2,
    facingRight :: Bool,
    walkPhase   :: Float}
    deriving (Eq)

data GoombaState = Walking | Flat Float | Dying
    deriving (Eq)

-- Characters!
data Goomba = Goomba {
    gPos   :: Vec2,
    gVel   :: Vec2,
    gDir   :: Float,
    gState :: GoombaState,
    gPhase :: Float}
    deriving (Eq)

data HammerBro = HammerBro {
    broPos      :: Vec2,
    broCooldown :: Float,
    broWindup   :: Maybe Float,
    broAnim     :: Float,
    broDead     :: Bool,
    broVel      :: Vec2}
    deriving (Eq)

data Hammer = Hammer {
    hamPos  :: Vec2,
    hamVel  :: Vec2,
    hamAnim :: Float}
    deriving (Eq)

data Boss = Boss {
    bossPos :: Vec2,
    bossHealth :: Int,
    bossCooldown :: Float,
    bossInvisible :: Float,
    bossVel :: Vec2,
    bossHomeY :: Float,
    bossJumping :: Bool,
    bossVolleys :: Int,
    bossAnim :: Float}
    deriving (Eq)
    
goombaW, goombaH :: Float
goombaW = 28
goombaH = 30

broW, broH, hammerSize :: Float
broW       = 36
broH       = 60
hammerSize = 20

bossW, bossH :: Float
bossW = 42
bossH = 72

spawnGoomba :: GoombaStart -> Goomba
spawnGoomba s = Goomba (gsCenter s) (Vec2 0 0) direction Walking 0
    where 
        direction = if gsDir s < 0 then -1 else 1

spawnBro :: BroStart -> HammerBro
spawnBro s = HammerBro (bsCenter s) 0 Nothing 0 False (Vec2 0 0)

spawnBoss :: Vec2 -> Boss
spawnBoss pos = Boss pos 3 1.5 0 (Vec2 0 0) (y pos) False 0 0

bossPhase :: Boss -> Int
bossPhase b = 4 - bossHealth b

data GameState = GameState {
    gsLevel        :: Level, 
    gsPlayerSelect :: PlayerSelect,
    gsMode         :: GameMode,
    gsStatus       :: GameStatus,
    gsPlayers      :: [Player],
    gsGoombas      :: [Goomba],
    gsBros         :: [HammerBro],
    gsHammers      :: [Hammer],
    gsBoss         :: Maybe Boss,
    gsScore        :: Int,
    gsStartScore   :: Int,
    gsClock        :: Float,
    gsCamY         :: Float,
    gsRise         :: Maybe Float,
    gsPaused       :: Bool,
    gsRng          :: Int,
    gsDebug        :: Bool}

-- Result: game won (T) or game over (F), with the final score.
data Screen = Menu | Playing GameState | Result Bool Int

type SpriteAtlas = [(String, Picture)]

data AppState = AppState {
    screen        :: Screen, 
    sprites       :: SpriteAtlas,
    playerSelect  :: PlayerSelect,
    message       :: Maybe String,
    highscores        :: [Highscore],
    seed          :: Int}

data Highscore = Highscore {
    hsLevel :: LevelId,
    hsScore :: Int }
    deriving (Eq)
    
initialApp :: SpriteAtlas -> AppState
initialApp atlas = AppState Menu atlas SinglePlayer Nothing [] 12345

defaultPlayer :: Player
defaultPlayer = Player { 
    playerPos = Vec2 0 0, 
    playerVel = Vec2 0 0, 
    playerW   = 32, 
    playerH   = 42,
    onGround  = False,
    playerIn  = noInput,
    playerDead  = False, 
    playerHealth = maxHealth,
    playerSpawn = Vec2 0 0,
    facingRight = True,
    walkPhase   = 0}

spawnPlayer :: PlayerStart -> Player
spawnPlayer ps = defaultPlayer { 
    playerPos = psCenter ps, 
    playerVel = psVel ps, 
    playerSpawn = psCenter ps}

mkGameState :: PlayerSelect -> Level ->  Either String GameState
mkGameState select = mkGameStateMode select LevelSelect

halfW, halfH :: Float
halfW = 400
halfH = 300

-- Sprites and textures are drawn at twice their pixel size.
pixelScale :: Float
pixelScale = 2

-- Random level: once a player jumps, the camera rises at riseSpeed for riseDuration
riseSpeed, riseDuration, finishTop :: Float
riseSpeed    = 50
riseDuration = 60
finishTop    = riseSpeed * riseDuration + 200

-- Hearts each player starts with.
maxHealth :: Int
maxHealth = 3

-- The score is also the timer
startScore :: Int
startScore = 300

mkGameStateMode :: PlayerSelect -> GameMode -> Level ->  Either String GameState
mkGameStateMode select mode lvl
    | length (lvlPlayers lvl) < selectPlayer select = Left "Level has too few starting positions." 
    | otherwise = Right (GameState {
        gsLevel        = lvl,
        gsPlayerSelect = select,
        gsMode         = mode,
        gsStatus       = InProgress,
        gsPlayers      = take (selectPlayer select) (map spawnPlayer (lvlPlayers lvl)),
        gsGoombas      = map spawnGoomba (lvlGoombas lvl),
        gsBros         = map spawnBro (lvlBros lvl),
        gsHammers      = [],
        gsBoss         = case lvlBosses lvl of
            [] -> Nothing
            pos : _ -> Just (spawnBoss pos),
        gsScore        = startScore,
        gsStartScore   = startScore,
        gsClock        = 0,
        gsCamY         = 0,
        gsRise         = Nothing,
        gsPaused       = False,
        gsRng          = 12345,
        gsDebug        = False})

class HasHitbox a where
    hitbox :: a -> Hitbox

instance HasHitbox Hitbox where
    hitbox = id

instance HasHitbox Player where
    hitbox p = Hitbox (playerPos p) (playerW p) (playerH p)

instance HasHitbox Goomba where
    hitbox gb = Hitbox (gPos gb) goombaW goombaH

instance HasHitbox HammerBro where
    hitbox b = Hitbox (broPos b) broW broH

instance HasHitbox Hammer where
    hitbox h = Hitbox (hamPos h) hammerSize hammerSize

instance HasHitbox Boss where
    hitbox b = Hitbox (bossPos b) bossW bossH

instance HasHitbox StaticObj where
    hitbox s = Hitbox (soCenter s) (soWidth s) (soHeight s)

-- Do two things touch?
touches :: (HasHitbox a, HasHitbox b) => a -> b -> Bool
touches a b = overlaps (hitbox a) (hitbox b)

-- Edges of a box.
hbTop, hbBottom, hbLeft, hbRight :: Hitbox -> Float
hbTop h    = y (hbCenter h) + hbHeight h / 2
hbBottom h = y (hbCenter h) - hbHeight h / 2
hbLeft h   = x (hbCenter h) - hbWidth h / 2
hbRight h  = x (hbCenter h) + hbWidth h / 2

soTop :: StaticObj -> Float
soTop = hbTop . hitbox

overlaps :: Hitbox -> Hitbox -> Bool
overlaps a b =
  abs (x (hbCenter a) - x (hbCenter b)) < (hbWidth a + hbWidth b) / 2 &&
  abs (y (hbCenter a) - y (hbCenter b)) < (hbHeight a + hbHeight b) / 2
