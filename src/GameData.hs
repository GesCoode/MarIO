module GameData where

import Graphics.Gloss (Picture)

-- | 2D point / vector. Object positions use the center.
data Vec2 = Vec2 
    { x :: Float
    , y :: Float
    } deriving (Eq, Show)

-- | AABB: center + width + height.
data Hitbox = Hitbox 
    { hbCenter :: Vec2
    , hbWidth :: Float
    , hbHeight :: Float
    } deriving (Eq, Show)

-- | Static level object (platform, block, …).
data StaticObj = StaticObj 
    { soCenter :: Vec2
    , soWidth :: Float
    , soHeight :: Float
    , soSprite :: String
    } deriving (Eq, Show)

-- | Start state and velocity
data PlayerStart = PlayerStart 
    { psCenter :: Vec2
    , psVel :: Vec2
    } deriving (Eq, Show)

data Direction = DirR | DirL
    deriving (Eq, Show)

data LevelId = Sandbox | Level1 | Level2 | Level3
    deriving (Eq, Show)

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

-- | Types of enemies 
data EnemyType = Goomba | Range | Boss
    deriving (Eq, Show)

data EnemyStart = EnemyStart
    { eiType :: EnemyType
    , eiCenter :: Vec2 
    } deriving (Eq, Show)

-- | Parsed level payload (level id comes from which file was loaded).
data LevelFile = LevelFile 
    { lfStatics :: [StaticObj]
    , lfPlayers :: [PlayerStart]
    , lfEnemies :: [EnemyStart]
    } deriving (Eq, Show)

data Level = Level 
    { lvlId :: LevelId
    , lvlStatics :: [StaticObj]
    , lvlPlayers :: [PlayerStart]
    , lvlEnemyStarts :: [EnemyStart]
    } deriving (Eq, Show)

-- | Independently track input for each player 
data InputHandler = InputHandler 
    { inLeft :: Bool
    , inRight :: Bool
    , inJump :: Bool 
    } deriving (Eq, Show)

noInput :: InputHandler
noInput = InputHandler False False False

data PlayerSelect = SinglePlayer | TwoPlayer
    deriving (Eq, Show)

selectPlayer :: PlayerSelect -> Int
selectPlayer SinglePlayer = 1
selectPlayer TwoPlayer =2 

data Player = Player 
    { playerPos :: Vec2
    , playerVel :: Vec2
    , playerW :: Float
    , playerH :: Float
    , playerHealth :: Int
    , onGround :: Bool
    , playerIn :: InputHandler
    , playerDir :: Direction
    } deriving (Eq, Show)

data Enemy = Enemy
    { enemyType :: EnemyType
    , enemyPos :: Vec2
    , enemyVel :: Vec2
    , enemyW :: Float
    , enemyH :: Float
    , enemyHealth :: Int
    , enemyCooldown :: Float
    , enemyMinX :: Float
    , enemyMaxX :: Float
    , enemyDir :: Direction
    } deriving (Eq, Show)
    
-- | Compared to enemy, boss has own health, phase, and countdown
data Boss = Boss
    { bossPos :: Vec2
    , bossHealth :: Int
    , bossPhase :: Int
    , bossCooldown :: Float
    } deriving (Eq, Show)

data Projectile = Projectile 
    { projectilePos :: Vec2
    , projectileVel :: Vec2
    , projectileDur :: Float
    , projectileDmg :: Int
    } deriving (Eq, Show)
    
    
-- Remove debug
data GameState = GameState
    { gsLevel :: Level
    , gsPlayerSelect :: PlayerSelect
    , gsPlayers :: [Player]
    , gsEnemies :: [Enemy]
    , gsBoss :: Maybe Boss
    , gsProjectiles :: [Projectile]
    , gsScore :: Int
    , gsPaused :: Bool
    , gsDebug :: Bool
    }

data Screen = Menu | Playing GameState

type SpriteAtlas = [(String, Picture)]

data AppState = AppState 
    { screen :: Screen
    , sprites :: SpriteAtlas
    , message :: Maybe String
    , playerSelect :: PlayerSelect }

initialApp :: SpriteAtlas -> AppState
initialApp atlas = AppState Menu atlas Nothing SinglePlayer

defaultPlayer :: Player
defaultPlayer = Player
    { playerPos = Vec2 0 0 
    , playerVel = Vec2 0 0
    , playerW = 32 -- Image is 16x21px, so double scale
    , playerH = 42 -- Image is 16x21px, so double scale
    , playerHealth = 3
    , onGround = False
    , playerIn = noInput
    , playerDir = DirR
    }

spawnPlayer :: PlayerStart -> Player
spawnPlayer start = defaultPlayer 
    { playerPos = psCenter start
    , playerVel = psVel start
    }

spawnEnemy :: EnemyStart -> Enemy 
spawnEnemy start = Enemy
    { enemyType     = eiType start
    , enemyPos      = eiCenter start
    , enemyVel      = Vec2 0 0 
    , enemyW        = 32 -- Adjust after finding img
    , enemyH        = 32 -- Adjust after finding img
    , enemyHealth   = 1
    , enemyCooldown = 1
    , enemyMinX     = x (eiCenter start) - 50 -- Adjust range
    , enemyMaxX     = x (eiCenter start) + 50 -- Adjust range
    , enemyDir      = DirR
    }

spawnBoss :: EnemyStart -> Boss
spawnBoss start = Boss (eiCenter start) 8 1 1.5

mkGameState :: PlayerSelect -> Level -> GameState
mkGameState select lvl = GameState
    { gsLevel = lvl
    , gsPlayerSelect = select
    , gsPlayers = take (selectPlayer select) players
    , gsEnemies = map spawnEnemy basicEnemies
    , gsBoss = case bosses of
                  []         -> Nothing
                  (boss : _) -> Just (spawnBoss boss)
    , gsProjectiles = []
    , gsScore = 0
    , gsPaused = False
    , gsDebug = True 
    }
    where
        players = map spawnPlayer (lvlPlayers lvl)
        basicEnemies = filter ((/= Boss) . eiType) (lvlEnemyStarts lvl)
        bosses = filter ((== Boss) . eiType) (lvlEnemyStarts lvl)
        
staticHitbox :: StaticObj -> Hitbox
staticHitbox object = Hitbox (soCenter object) (soWidth object) (soHeight object)

playerHitbox :: Player -> Hitbox
playerHitbox player = Hitbox (playerPos player) (playerW player) (playerH player)

enemyHitbox :: Enemy -> Hitbox
enemyHitbox enemy = Hitbox (enemyPos enemy)  (enemyW enemy) (enemyH enemy)

projectileHitbox :: Projectile -> Hitbox 
projectileHitbox projectile = Hitbox (projectilePos projectile) 10 10 -- Adjust

-- | AABB-overlap: collision at edge does not count as overlap
overlaps :: Hitbox -> Hitbox -> Bool
overlaps a b =
  abs (x (hbCenter a) - x (hbCenter b)) < (hbWidth a + hbWidth b) / 2 &&
  abs (y (hbCenter a) - y (hbCenter b)) < (hbHeight a + hbHeight b) / 2