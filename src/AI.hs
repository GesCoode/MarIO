module AI (
    stepEnemies,
    gravity,
    maxFallSpeed,
    removeBelowY) where

import Data.List (mapAccumL, minimumBy)
import Data.Ord (comparing)

import GameData ( 
    GameState (..),
    Vec2 (..),
    Player (..),
    Goomba (..),
    GoombaState (..),
    HammerBro (..),
    Hammer (..),
    Hitbox (..),
    Boss (..),
    bossW,
    goombaH,
    goombaW,
    goombaHitbox,
    overlaps,
    playerDead, 
    playerSpawn)

{-
  Adapted and was inspired by the following sources: 
- https://openstax.org/books/university-physics-volume-1/pages/3-key-equations
- https://www.gafferongames.com/post/integration_basics/
- https://openstax.org/books/university-physics-volume-1/pages/4-3-projectile-motion
- https://developer.mozilla.org/en-US/docs/Games/Techniques/2D_collision_detection
- https://mathworld.wolfram.com/Distance.html
- https://www.gnu.org/software/gsl/doc/html/rng.html
- https://www.gafferongames.com/post/fix_your_timestep/
- https://box2d.org/documentation/md_collision.html
- https://gameprogrammingpatterns.com/state.html
- https://gameprogrammingpatterns.com/update-method.html
-}

-- | Constants (also used in Physics) 
-- Adjust if necessary
gravity, maxFallSpeed, removeBelowY :: Float
gravity = 1200
maxFallSpeed = 800
removeBelowY = -400

-- | Update enemies 
stepEnemies :: Float -> [Hitbox] -> GameState -> GameState
stepEnemies dt pltfrms gs = gs {
    gsGoombas = filter visGoomba goombas,
    gsBros = filter visBro bros,
    gsHammers = filter visHammer (movingHammer ++ newHammers ++ bossHammers),
    gsBoss = newBoss,
    gsRng = newSeed}
    where 
        players = gsPlayers gs
        (newSeed, goombas) = mapAccumL (stepGoomba dt pltfrms) (gsRng gs) (gsGoombas gs)
        (bros, throws) = unzip (map (stepBro dt players) (gsBros gs))
        newHammers = concat throws
        movingHammer = map (stepHammer dt) (gsHammers gs)
        (newBoss, bossHammers) = case gsBoss gs of
            Nothing -> (Nothing, [])
            Just boss -> 
                let (boss', hammers) = stepBoss dt pltfrms players boss
                in (Just boss', hammers) 

-- Niet heel netjes want gecopied uit Procedural
nextSeed :: Int -> Int
nextSeed n = (n * 1103515245 + 12345) `mod` 2147483648

numberWithinRange :: Float -> Float -> Int -> Float
numberWithinRange low high seed = low + (high - low) * fromIntegral seed / 2147483648

goombaSpeed, walkDistance :: Float
goombaSpeed = 60
walkDistance = 20

-- | Goomba walks, falls, gets squished or disappears after death
stepGoomba :: Float -> [Hitbox] -> Int -> Goomba -> (Int, Goomba)
stepGoomba dt pltfrms seed goomba = case gState goomba of 
    Walking -> (seed, moveGoomba dt pltfrms goomba)
    Flat rest 
        | rest > dt -> (seed, goomba { gState = Flat (rest - dt) })
        | otherwise -> 
            let next = nextSeed seed
                finalSeed = nextSeed next
                vx = numberWithinRange (-200) 200 next
                vy = numberWithinRange 350 600 finalSeed
            in (finalSeed, goomba { gState = Dying, gVel = Vec2 vx vy })
    Dying -> 
        let Vec2 px py = gPos goomba 
            Vec2 vx vy = gVel goomba
            vy' = vy - gravity * dt
        in (seed, goomba {
            gPos = Vec2 (px + vx * dt) (py + vy' * dt),
            gVel = Vec2 vx vy' })

-- | Turn when edge or wall
moveGoomba :: Float -> [Hitbox] -> Goomba -> Goomba
moveGoomba dt pltfrms goomba = goomba {
    gPos = pos2,
    gVel = Vec2 (dir2 * goombaSpeed) vertSpeed2,
    gDir = dir2, 
    gPhase = gPhase goomba + goombaSpeed * dt / walkDistance}
    where 
        Vec2 px py = gPos goomba
        direction = gDir goomba
        speedY = max (-maxFallSpeed) (y (gVel goomba) - gravity * dt)
        newX = Vec2 (px + direction * goombaSpeed * dt) py
        hitboxAt point = (goombaHitbox goomba) { hbCenter = point}
        hits box = any (overlaps box) pltfrms
        onPltfrm = hits (hitboxAt (Vec2 px (py - 2)))
        feet = Vec2 (x newX + direction * goombaW / 2) (py - goombaH / 2 - 2)
        offEdge = onPltfrm && not (hits (Hitbox feet 2 2))
        turn = hits (hitboxAt newX) || offEdge
        dir2 = if turn then -direction else direction
        pos1 = if turn then gPos goomba else newX
        newY = Vec2 (x pos1) (y pos1 + speedY * dt)
        (pos2, vertSpeed2)
            | hits (hitboxAt newY) = (pos1, 0)
            | otherwise = (newY, speedY)
 
-- | Nearest living player
nearestPlayer :: Vec2 -> [Player] -> Maybe Vec2
nearestPlayer o players = case p of 
    [] -> Nothing
    _ -> Just (snd (minimumBy (comparing fst) p))
    where 
        p = [(distQuad o (playerPos player), playerPos player)
            | player <- players, not (playerDead player)]       

distQuad :: Vec2 -> Vec2 -> Float
distQuad (Vec2 ax ay) (Vec2 bx by) = (ax - bx) ^ (2 :: Int) + (ay - by) ^ (2 :: Int)
    
-- | Throw hammer with parabola 
throwAt :: Vec2 -> Vec2 -> Hammer
throwAt (Vec2 sx sy) (Vec2 tx ty) = Hammer (Vec2 sx sy) (Vec2 vx vy) 0
    where 
        dx = tx - sx
        dy = ty - sy
        airtime = max 0.6 (min 1.2 (abs dx / 350))
        vx = dx / airtime
        vy = dy / airtime + 0.5 * gravity * airtime

broReach, broInterval, windUp :: Float
broReach = 440
broInterval = 2.4
windUp = 0.4

-- | HammerBro chooses target and preps throw 
stepBro :: Float -> [Player] -> HammerBro -> (HammerBro, [Hammer])
stepBro dt players bro 
    | broDead bro = (bro {
        broPos = Vec2 (px + vx * dt) (py + newVy * dt),
        broVel = Vec2 vx newVy},
        [])
    | otherwise = case broWindup bro of 
        Just secs
            | secs > dt -> (withAnim { broWindup = Just (secs - dt) }, [])
            | otherwise -> (
                withAnim { 
                    broWindup = Nothing, 
                    broCooldown = broInterval },
                maybeThrow targets )
        Nothing 
            | broCooldown bro > 0 -> (withAnim { broCooldown = max 0 (broCooldown bro - dt) }, [])
            | Just _ <- targets -> (withAnim { broWindup = Just windUp }, [])
            | otherwise -> (withAnim, [])
    where 
        Vec2 px py = broPos bro
        Vec2 vx vy = broVel bro
        newVy = vy - gravity * dt
        withAnim = bro { broAnim = broAnim bro + dt }
        -- he only faces left, so only players on his left count
        onLeft = [p | p <- players, x (playerPos p) < x (broPos bro)]
        targets = do 
            target <- nearestPlayer (broPos bro) onLeft
            if distQuad (broPos bro) target <= broReach * broReach
                then Just target else Nothing
        hand = Vec2 (x (broPos bro) - 10) (y (broPos bro) + 30)
        maybeThrow Nothing = []
        maybeThrow (Just target) = [throwAt hand target]

-- | Second boss has shorter interval and two hammers
-- Boss timings (seconds) and jump
bossThrowGap, bossJumpGap, bossVolleyGap, bossRest, bossAirTime :: Float
bossThrowGap  = 1.8   -- phase 1: one hammer every 1.8 s
bossJumpGap   = 3.0   -- phase 2: a jump every 3 s
bossVolleyGap = 0.35  -- phase 3: time between the pairs of hammers
bossRest      = 1.5   -- phase 3: rest after landing, before the next burst
bossAirTime   = 1.6   -- a jump takes 1.6 s: high and slow, so the player can react

-- | Phase 1 (3 health): throws single hammers.
--   Phase 2 (2 health): only jumps at the player.
--   Phase 3 (1 health): 3 quick pairs of hammers, then jumps at the player, rests, repeats.
stepBoss :: Float -> [Hitbox] -> [Player] -> Boss -> (Boss, [Hammer])
stepBoss dt pltfrms players boss 
    | bossHealth boss <= 0 = (boss, [])
    | bossJumping boss = (fly ticked, [])
    | bossCooldown ticked > 0 = (ticked, [])
    | otherwise = case nearestPlayer (bossPos boss) players of
        Nothing -> (ticked { bossCooldown = 0.4 }, [])
        Just target -> case bossPhase boss of
            1 -> (ticked { bossCooldown = bossThrowGap }, [throwAt hand target])
            2 -> (jumpAt target ticked, [])
            _ | bossVolleys boss < 2 ->
                    ( ticked { bossCooldown = bossVolleyGap, bossVolleys = bossVolleys boss + 1 }
                    , pair target )
              | otherwise ->   -- third pair: throw it and jump straight after
                    ( jumpAt target ticked { bossVolleys = 0 }, pair target )
    where 
        ticked = boss {
            bossAnim = bossAnim boss + dt,
            bossCooldown = max 0 (bossCooldown boss - dt),
            bossInvisible = max 0 (bossInvisible boss - dt) }
        Vec2 bx by = bossPos boss
        hand = Vec2 bx (by + 35)
        -- two hammers at once, landing just in front of and just behind the player
        pair (Vec2 tx ty) = [throwAt hand (Vec2 (tx - 40) ty), throwAt hand (Vec2 (tx + 40) ty)]

        -- big parabola that comes down on the target's x, at his own floor height
        jumpAt (Vec2 tx _) b = b {
            bossJumping = True,
            bossVel = Vec2 ((landX tx - bx) / bossAirTime) (0.5 * gravity * bossAirTime) }

        -- never land past the edge of the floor (e.g. when the player is mid-air over the gap)
        landX tx
            | null pltfrms = tx
            | otherwise    = max (left + bossW / 2) (min (right - bossW / 2) tx)
        left  = minimum [x (hbCenter h) - hbWidth h / 2 | h <- pltfrms]
        right = maximum [x (hbCenter h) + hbWidth h / 2 | h <- pltfrms]

        fly b =
            let Vec2 px py = bossPos b
                Vec2 vx vy = bossVel b
                vy' = vy - gravity * dt
                ny  = py + vy' * dt
            in if vy' < 0 && ny <= bossHomeY b
                then b {   -- landed
                    bossPos = Vec2 (px + vx * dt) (bossHomeY b),
                    bossVel = Vec2 0 0,
                    bossJumping = False,
                    bossCooldown = if bossPhase b == 2 then bossJumpGap else bossRest }
                else b { bossPos = Vec2 (px + vx * dt) ny, bossVel = Vec2 vx vy' }

stepHammer :: Float -> Hammer -> Hammer
stepHammer dt hammer = hammer { 
    hamPos = Vec2 (px + vx * dt) (py + vy' * dt),
    hamVel = Vec2 vx vy',
    hamAnim = hamAnim hammer + dt }
    where 
        Vec2 px py = hamPos hammer
        Vec2 vx vy = hamVel hammer
        vy' = vy - gravity * dt
      
visGoomba :: Goomba -> Bool
visGoomba enemy = 
    not (gState enemy == Dying &&
        y (gPos enemy) < removeBelowY) 

visBro :: HammerBro -> Bool
visBro enemy = 
    not (broDead enemy &&
        y (broPos enemy) < removeBelowY) 

visHammer :: Hammer -> Bool
visHammer hammer = 
    y (hamPos hammer) > removeBelowY 