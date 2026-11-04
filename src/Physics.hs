module Physics
    ( stepWorld
    ) where

import Data.List (foldl', mapAccumL, minimumBy)
import Data.Ord (comparing)

import GameData
  ( GameState (..)
  , Goomba (..)
  , Hammer (..)
  , HammerBro (..)
  , GoombaState (..)
  , Hitbox (..)
  , InputHandler (..)
  , Level (..)
  , Player (..)
  , Vec2 (..)
  , overlaps
  , playerHitbox
  , staticHitbox
  , goombaHitbox
  , broHitbox
  , hammerHitbox
  , goombaH
  , goombaW
  , defaultPlayer
  )
  
-- | One frame: players, goombas, then player/goomba contact.
stepWorld :: Float -> GameState -> GameState
stepWorld dt gs 
    | gsPaused gs = gs
    | otherwise = gs { 
        gsPlayers = ps3,
        gsGoomas = filter onScreen gbs2,
        gsRng = rng1,
        gsBros = filter broOnScreen bros2,
        gsHammers = filter hammerOnScreen (hams1 ++ thrown)}
    where 
        solids = map staticHitbox (lvlStatics (gsLevel gs))
        -- advancing the seed every frame makes kill timing part of the randomness
        (rng1, gbs1) = mapAccumL (stepGoomba dt solids) (nextRng (gsRng gs)) (gsGoombas gs)
        ps1          = map (stepPlayer dt solids) (gsPlayers gs)
        (ps2, gbs2)  = resolveHits ps1 gbs1
        -- hammer bros look at the players after movement; new hammers start next frame
        (bros1, thrownM) = unzip (map (stepBro dt ps2) (gsBros gs))
        thrown           = [h | Just h <- thrownM]
        hams1            = map (stepHammer dt) (gsHammers gs)
        -- stomping a bro kills him (same rule as goombas)
        (ps2b, bros2) = stompBros ps2 bros1
        -- touching a hammer or a living bro kills
        dangers      = map hammerHitbox hams1 ++ [broHitbox b | b <- bros2, not (broDead b)]
        ps3          = map (hurtBy dangers) ps2b
        onScreen gb  = not (gState gb == Dying && y (gPos gb) < killY)

-- | Move player along x- and y-axis, or play deathsprite
stepPlayer :: Float -> [Hitbox] -> Player -> Player
stepPlayer dt solids pl0
    | playerDead pl0 = fall pl0
    | otherwise      = respawnIfFallen (updateAnim dt (moveY (moveX (updateVel dt (playerIn pl0) pl0))))
    where
        hits pl = any (overlaps (playerHitbox pl)) solids
        -- dead: just gravity, no collisions, until off screen
        fall pl =
            let Vec2 px py = playerPos pl
                Vec2 vx vy = playerVel pl
                vy' = vy - gravity * dt
                pl1 = pl { 
                    playerPos = Vec2 (px + vx * dt) (py + vy' * dt), 
                    playerVel = Vec2 vx vy' }
            in respawnIfFallen pl1

    -- Fallen spplayer falls without collisions out of frame
        respawnIfFallen pl
            | y (playerPos pl) < killY = defaultPlayer { 
                                             playerPos = playerSpawn pl,
                                             playerSpawn = playerSpawn pl,
                                             playerIn = playerIn pl }
            | otherwise = pl

        moveX pl =
            let Vec2 px py = playerPos pl
                Vec2 vx vy = playerVel pl
                pl1 = pl { playerPos = Vec2 (px + vx * dt) py }
            in if hits pl1 then pl { playerVel = Vec2 0 vy } else pl1

        moveY pl =
            let Vec2 px py = playerPos pl
                Vec2 vx vy = playerVel pl
                pl1 = pl { playerPos = Vec2 px (py + vy * dt) }
            in if hits pl1
                 then pl { playerVel = Vec2 vx 0, onGround = vy < 0 }
                 else pl1 { onGround = False }

-- Constants
accel, maxSpeed, gravity, friction, jumpSpeed, maxFallSpeed :: Float
accel = 1500
maxSpeed = 300
gravity = 1200
friction = 1800
jumpSpeed = 550
maxFallSpeed = 800

updateVel :: Float -> InputHandler -> Player -> Player
updateVel dt inp p = p {playerVel = Vec2 vx4 vy2}
    where
        Vec2 vx0 vy0 = playerVel p
        dir = (if inRight inp then 1 else 0) - (if inLeft inp then 1 else 0)
        vx1 = vx0 + dir * accel * dt
        
        vx2 
            | dir == 0 && onGround p = signum vx1 * max 0 (abs vx1 - friction * dt)
            | otherwise = vx1
        
        vx4 = max (-maxSpeed) (min maxSpeed vx2)
        
        vy1 
            | inJump inp && onGround p = jumpSpeed
            | otherwise = vy0
            
        vy2 = max (-maxFallSpeed) (vy1 - gravity * dt)

-- Pixels travelled per walk frame: higher = slower animation.
stepLength :: Float
stepLength = 20

-- Advance the walk cycle by distance travelled (so faster = quicker animation),
-- only while on the ground. Standing still resets to the idle frame.
updateAnim :: Float -> Player -> Player
updateAnim dt p = p { facingRight = facing, walkPhase = phase }
    where
        vx = x (playerVel p)
        facing 
            | vx >  1   = True
            | vx < -1   = False
            | otherwise = facingRight p
        phase 
            | not (onGround p) = walkPhase p
            | abs vx < 1       = 0
            | otherwise        = walkPhase p + abs vx * dt / stepLength

-- ===================== Goombas & contact =====================

goombaSpeed, flatTime, stompBounce, deathHop, killY :: Float
goombaSpeed = 60     -- walking speed
flatTime    = 0.4    -- seconds squashed before flying off
stompBounce = 350    -- player bounce after a stomp
deathHop    = 450    -- player's upward hop when killed
killY       = -400   -- below the screen (window is 600 high, centered)

-- Tiny LCG so we need no extra package. Returns the next seed.
nextRng :: Int -> Int
nextRng s = (s * 1103515245 + 12345) `mod` 2147483648

-- Seed -> value in [lo, hi).
rngRange :: Float -> Float -> Int -> Float
rngRange lo hi s = lo + (hi - lo) * fromIntegral s / 2147483648

stepGoomba :: Float -> [Hitbox] -> Int -> Goomba -> (Int, Goomba)
stepGoomba dt solids rng gb = case gState gb of
    Walking -> (rng, walk gb)
    Flat t
        | t - dt > 0 -> (rng, gb { gState = Flat (t - dt) })
        | otherwise  ->
        -- launch in a random parabola
            let r1 = nextRng rng
                r2 = nextRng r1
                vx = rngRange (-200) 200 r1
                vy = rngRange 350 600 r2
            in (r2, gb { gState = Dying, gVel = Vec2 vx vy })
    Dying ->
        let Vec2 px py = gPos gb
            Vec2 vx vy = gVel gb
            vy' = vy - gravity * dt
        in (rng, gb { gPos = Vec2 (px + vx * dt) (py + vy' * dt), gVel = Vec2 vx vy' })
    where
        hits h = any (overlaps h) solids
        boxAt p = (goombaHitbox gb) { hbCenter = p }
        below (Vec2 px py) = Vec2 px (py - 2)

    walk g =
          let Vec2 px py = gPos g
              dir        = gDir g
              vx         = dir * goombaSpeed
              vy         = max (-maxFallSpeed) (y (gVel g) - gravity * dt)
              -- horizontal: flip on a wall, or at a ledge when standing on ground
              nextX      = Vec2 (px + vx * dt) py
              grounded   = hits (boxAt (below (gPos g)))
              -- small probe just under the front foot
              footProbe  = Hitbox (Vec2 (x nextX + dir * goombaW / 2) (py - goombaH / 2 - 2)) 2 2
              ledge      = grounded && not (hits footProbe)
              (pos1, dir1)
                  | hits (boxAt nextX) || ledge = (gPos g, -dir)
                  | otherwise                   = (nextX, dir)
              -- vertical
              nextY      = Vec2 (x pos1) (y pos1 + vy * dt)
              (pos2, vy1)
                  | hits (boxAt nextY) = (pos1, 0)
                  | otherwise          = (nextY, vy)
          in g { 
              gPos = pos2, 
              gVel = Vec2 vx vy1, 
              gDir = dir1, 
              gPhase = gPhase g + abs vx * dt / stepLength }

-- Each walking goomba checks the players in order; first touching player wins.
resolveHits :: [Player] -> [Goomba] -> ([Player], [Goomba])
resolveHits ps []         = (ps, [])
resolveHits ps (gb : gbs) =
    let (ps1, gb1)  = hitGoomba ps gb
        (ps2, gbs1) = resolveHits ps1 gbs
    in (ps2, gb1 : gbs1)

hitGoomba :: [Player] -> Goomba -> ([Player], Goomba)
hitGoomba [] gb = ([], gb)
hitGoomba (p : ps) gb
    | gState gb /= Walking = (p : ps, gb)
    | playerDead p || not (overlaps (playerHitbox p) (goombaHitbox gb)) =
        let (ps', gb') = hitGoomba ps gb 
        in (p : ps', gb')
    | isStomp p (gPos gb) = 
        (bounce p : ps, gb { gState = Flat flatTime, gVel = Vec2 0 0 })
    | otherwise = (killPlayer p : ps, gb)

-- Falling, with feet in the upper half of the enemy (whose center is given).
isStomp :: Player -> Vec2 -> Bool
isStomp p enemyCenter = y (playerVel p) < 0 && feet > y enemyCenter
  where 
      feet = y (playerPos p) - playerH p / 2

bounce :: Player -> Player
bounce pl = pl { 
    playerVel = Vec2 (x (playerVel pl)) stompBounce, 
    onGround = False }

killPlayer :: Player -> Player
killPlayer pl = pl { 
    playerDead = True, 
    playerVel = Vec2 0 deathHop, 
    onGround = False }

hurtBy :: [Hitbox] -> Player -> Player
hurtBy dangers p
  | not (playerDead p) && any (overlaps (playerHitbox p)) dangers = killPlayer p
  | otherwise = p

-- ===================== Hammer Bros =====================

broRange, throwInterval, windupTime :: Float
broRange      = 400   -- how close (distance) a player must be
throwInterval = 2.4   -- one throw every 2.4 s while a player is in range ...
windupTime    = 0.4   -- ... of which 0.4 s in the throw pose (HT)

-- Closest living player on his left (he only faces left) within range.
broTarget :: HammerBro -> [Player] -> Maybe Vec2
broTarget b ps = case candidates of
    [] -> Nothing
    cs -> Just (snd (minimumBy (comparing fst) cs))
    where
        Vec2 bx by = broPos b
        candidates =
            [ (d, pos)
            | p <- ps, not (playerDead p),
            let pos@(Vec2 px py) = playerPos p,
            px < bx,
            let d = sqrt ((px - bx) ^ (2 :: Int) + (py - by) ^ (2 :: Int)),
            d <= broRange ]

stepBro :: Float -> [Player] -> HammerBro -> (HammerBro, Maybe Hammer)
stepBro dt ps b
    | broDead b =   -- just falls, no collisions
        let Vec2 px py = broPos b
            Vec2 vx vy = broVel b
            vy' = vy - gravity * dt
        in (b { broPos = Vec2 (px + vx * dt) (py + vy' * dt), broVel = Vec2 vx vy' }, Nothing)
    | otherwise = 
        case broWindup b of
            Just t
                | t - dt > 0 -> (b' { broWindup = Just (t - dt) }, Nothing)
                | otherwise  ->   -- release
                    ( b' { broWindup = Nothing, broCooldown = throwInterval - windupTime },
                     fmap (throwAt hand) (broTarget b ps))
            Nothing
                | broCooldown b > 0 -> (b' { broCooldown = broCooldown b - dt }, Nothing)
                | Just _ <- broTarget b ps -> (b' { broWindup = Just windupTime }, Nothing)
                | otherwise -> (b', Nothing)
    where
        b'   = b { broAnim = broAnim b + dt }
        hand = Vec2 (x (broPos b) - 10) (y (broPos b) + 30)

-- Launch so the hammer passes through the target after time t.
throwAt :: Vec2 -> Vec2 -> Hammer
throwAt (Vec2 sx sy) (Vec2 tx ty) = Hammer (Vec2 sx sy) (Vec2 vx vy) 0
    where
        dx = tx - sx
        dy = ty - sy
        t  = max 0.6 (min 1.2 (abs dx / 350))
        vx = dx / t
        vy = dy / t + 0.5 * gravity * t

-- Hammers fly through blocks.
stepHammer :: Float -> Hammer -> Hammer
stepHammer dt h = h { hamPos = Vec2 (px + vx * dt) (py + vy' * dt), hamVel = Vec2 vx vy', hamAnim = hamAnim h + dt }
    where
        Vec2 px py = hamPos h
        Vec2 vx vy = hamVel h
        vy' = vy - gravity * dt

hammerOnScreen :: Hammer -> Bool
hammerOnScreen h = y (hamPos h) > killY && abs (x (hamPos h)) < 600

broOnScreen :: HammerBro -> Bool
broOnScreen b = not (broDead b && y (broPos b) < killY)

-- Each living bro checks the players in order; the first one stomping him wins.
-- (Touching him any other way is handled by hurtBy.)
stompBros :: [Player] -> [HammerBro] -> ([Player], [HammerBro])
stompBros ps []       = (ps, [])
stompBros ps (b : bs) =
    let (ps1, b1)  = stompBro ps b
        (ps2, bs1) = stompBros ps1 bs
    in (ps2, b1 : bs1)

stompBro :: [Player] -> HammerBro -> ([Player], HammerBro)
stompBro [] b = ([], b)
stompBro (p : ps) b
    | broDead b = (p : ps, b)
    | not (playerDead p) 
        && overlaps (playerHitbox p) (broHitbox b) 
        && isStomp p (broPos b) = 
        ( bounce p : ps
        , b { broDead = True, broWindup = Nothing, broVel = Vec2 0 broDeathHop })
    | otherwise = 
        let (ps', b') = stompBro ps b 
        in (p : ps', b')

broDeathHop :: Float
broDeathHop = 300   -- small hop up before falling off screen
