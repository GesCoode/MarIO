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
                let (boss', hammers) = stepBoss dt players boss
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
        p = [(distQuad o (playerSpawn player), playerSpawn player)
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
        targets = do 
            target <- nearestPlayer (broPos bro) players
            if distQuad (broPos bro) target <= broReach * broReach
                then Just target else Nothing
        hand = Vec2 (x (broPos bro) - 10) (y (broPos bro) + 30)
        maybeThrow Nothing = []
        maybeThrow (Just target) = [throwAt hand target]

-- | Second boss has shorter interval and two hammers
stepBoss :: Float -> [Player] -> Boss -> (Boss, [Hammer])
stepBoss dt players boss 
    | bossHealth boss <= 0 = (boss, [])
    | bossCooldown boss > dt = (cooldown, [])
    | otherwise = case nearestPlayer (bossPos boss) players of
        Nothing -> (cooldown { bossCooldown = 0.4 }, [])
        Just target -> ( 
            cooldown { bossCooldown = interval },
            map (`throwAt` target) targetpoints )
    where 
        cooldown = boss {
            bossCooldown = max 0 (bossCooldown boss - dt),
            bossInvisible = max 0 (bossInvisible boss - dt) }
        interval = if bossPhase boss == 1 then 1.8 else 0.9
        Vec2 bx by = bossPos boss
        targetpoints = if bossPhase boss == 1
            then [Vec2 bx (by + 35)]
            else [Vec2 (bx - 20) (by + 35), Vec2 (bx + 20) (by + 35)]

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
    y (hamPos hammer) > removeBelowY &&
    abs (x (hamPos hammer)) < 600 