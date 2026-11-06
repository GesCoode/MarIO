module Physics (stepWorld, findSwitch, goalHitbox) where

import Data.List (find, findIndex, mapAccumL, maximumBy)
import Data.Ord (comparing)

import AI (stepEnemies, gravity, maxFallSpeed, removeBelowY)
import GameData
import Procedural (finishPlatform)

stepWorld :: Float -> GameState -> GameState
stepWorld dt gs
    | gsPaused gs || gsStatus gs /= InProgress = gs
    | otherwise = randomLevel t (countDown t (iterate (step h) gs !! n))
    where
        t = max 0 (min dt 0.1)
        n = max 1 (ceiling (t / (1 / 120)))
        h = t / fromIntegral n

countDown :: Float -> GameState -> GameState
countDown dt gs = gs { gsScore = max 0 (gsScore gs - ticks), gsClock = clock - fromIntegral ticks }
    where
        clock = gsClock gs + dt
        ticks = floor clock

-- =========== One small step =================

step :: Float -> GameState -> GameState
step dt gs
    | gsStatus gs /= InProgress = gs
    | otherwise = moved {
        gsPlayers = players,
        gsGoombas = goombas,
        gsBros    = bros,
        gsBoss    = boss,
        gsScore   = gsScore gs + pts1 + pts2 + pts3,
        gsStatus  = status }
    where
        lvl    = gsLevel gs
        solids = map staticHitbox (lvlStatics lvl)
        oneWay = map staticHitbox (lvlPlatforms lvl)
        (bottomY, respawnAt) = fallLimits gs

        ps0   = map (stepPlayer dt solids oneWay bottomY respawnAt) (gsPlayers gs)
        moved = stepEnemies dt (solids ++ oneWay) gs { gsPlayers = ps0 }

        -- stomps and touching enemies
        (ps1, goombas, pts1) = forEach hitGoomba ps0 (gsGoombas moved)
        (ps2, bros,    pts2) = forEach stompBro  ps1 (gsBros moved)
        (ps3, boss,    pts3) = forEach hitBoss   ps2 (gsBoss moved)
        dangers = map hammerHitbox (gsHammers moved)
               ++ [ broHitbox b  | b <- bros, not (broDead b) ]
               ++ [ bossHitbox b | Just b <- [boss], bossHealth b > 0, bossInvisible b <= 0 ]
        players = map (hurtBy dangers) ps3

        alive = filter (not . playerDead) players
        won   = any (\p -> any (overlaps (playerHitbox p) . goalHitbox) (lvlGoals lvl)) alive
             || any ((<= 0) . bossHealth) boss
             || finishShown gs && any ((>= finishTop) . hbBottom . playerHitbox) alive
        status
            | not (null players) && all ((<= 0) . playerHealth) players = Loss
            | won       = Win
            | otherwise = InProgress

goalHitbox :: Vec2 -> Hitbox
goalHitbox point = Hitbox point 32 70

-- ============== Players ===================

stepPlayer :: Float -> [Hitbox] -> [Hitbox] -> Float -> (Player -> Vec2) -> Player -> Player
stepPlayer dt solids oneWay bottomY respawnAt p
    | y (playerPos p') >= bottomY = p'
    | otherwise = comeBack (killPlayer p')
    where
        p' | playerDead p = fall dt p
           | otherwise    = updateAnim dt (moveY dt solids oneWay (moveX dt solids (updateVel dt p)))
        comeBack q
            | playerHealth q > 0 = q { playerPos = respawnAt q, playerVel = Vec2 0 0,
                                       playerDead = False, onGround = False, walkPhase = 0 }
            | otherwise = q

fall :: Float -> Player -> Player
fall dt p = p { playerPos = Vec2 (px + vx * dt) (py + vy' * dt), playerVel = Vec2 vx vy' }
    where
        Vec2 px py = playerPos p
        Vec2 vx vy = playerVel p
        vy' = vy - gravity * dt

moveX :: Float -> [Hitbox] -> Player -> Player
moveX dt solids p = case find (overlaps (playerHitbox p')) solids of
    Nothing  -> p'
    Just box -> p { playerPos = Vec2 (side box) py, playerVel = Vec2 0 vy }
    where
        Vec2 px py = playerPos p
        Vec2 vx vy = playerVel p
        p' = p { playerPos = Vec2 (px + vx * dt) py }
        side box
            | vx > 0    = hbLeft box - playerW p / 2
            | vx < 0    = hbRight box + playerW p / 2
            | otherwise = px

-- Move up or down
moveY :: Float -> [Hitbox] -> [Hitbox] -> Player -> Player
moveY dt solids oneWay p = case (find (overlaps (playerHitbox p')) solids, landOn oneWay p p') of
    (Just box, _)       -> p { playerPos = Vec2 px (side box), playerVel = Vec2 vx 0, onGround = vy < 0 }
    (Nothing, Just top) -> p { playerPos = Vec2 px (top + playerH p / 2), playerVel = Vec2 vx 0, onGround = True }
    (Nothing, Nothing)  -> p' { onGround = False }
    where
        Vec2 px py = playerPos p
        Vec2 vx vy = playerVel p
        p' = p { playerPos = Vec2 px (py + vy * dt) }
        side box
            | vy > 0    = hbBottom box - playerH p / 2
            | vy < 0    = hbTop box + playerH p / 2
            | otherwise = py

landOn :: [Hitbox] -> Player -> Player -> Maybe Float
landOn platforms before after
    | y (playerVel before) >= 0 = Nothing
    | otherwise = case [ hbTop h | h <- platforms, feet before >= hbTop h - 0.01, feet after < hbTop h, under h ] of
        [] -> Nothing
        ts -> Just (maximum ts)
    where
        feet = hbBottom . playerHitbox
        under h = abs (x (hbCenter h) - x (playerPos after)) < (hbWidth h + playerW after) / 2

-- Velocity computations
updateVel :: Float -> Player -> Player
updateVel dt p = p { playerVel = Vec2 vx vy }
    where
        keys = playerIn p
        Vec2 vx0 vy0 = playerVel p
        dir  = fromIntegral (fromEnum (inRight keys) - fromEnum (inLeft keys))
        sped = vx0 + dir * accel * dt
        slowed
            | dir == 0 && onGround p = signum sped * max 0 (abs sped - friction * dt)
            | otherwise = sped
        vx = max (-maxSpeed) (min maxSpeed slowed)
        jumped = if inJump keys && onGround p then jumpSpeed else vy0
        vy = max (-maxFallSpeed) (jumped - gravity * dt)

accel, maxSpeed, friction, jumpSpeed :: Float
accel     = 1500
maxSpeed  = 300
friction  = 1800
jumpSpeed = 627 -- See if this holds

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
            | otherwise        = walkPhase p + abs vx * dt / 20

-- =================== Players vs enemies =====================

forEach :: Traversable t => ([Player] -> e -> ([Player], e, Int)) -> [Player] -> t e -> ([Player], t e, Int)
forEach f ps enemies = (ps', enemies', pts)
    where
        ((ps', pts), enemies') = mapAccumL one (ps, 0) enemies
        one (qs, n) e = let (qs', e', k) = f qs e in ((qs', n + k), e')

firstPlayer :: (Player -> Bool) -> Hitbox -> [Player] -> Maybe Int
firstPlayer ok box = findIndex (\p -> not (playerDead p) && overlaps (playerHitbox p) box && ok p)

updateAt :: Int -> (Player -> Player) -> [Player] -> [Player]
updateAt i f ps = [ if j == i then f p else p | (j, p) <- zip [0 ..] ps ]

isStomp :: Player -> Vec2 -> Bool
isStomp p enemy = y (playerVel p) < 0 && hbBottom (playerHitbox p) > y enemy

hitGoomba :: [Player] -> Goomba -> ([Player], Goomba, Int)
hitGoomba ps gb
    | gState gb /= Walking = (ps, gb, 0)
    | otherwise = case firstPlayer (const True) (goombaHitbox gb) ps of
        Nothing -> (ps, gb, 0)
        Just i
            | isStomp (ps !! i) (gPos gb) -> (updateAt i bounce ps, gb { gState = Flat 0.4, gVel = Vec2 0 0 }, 100)
            | otherwise                   -> (updateAt i killPlayer ps, gb, 0)

stompBro :: [Player] -> HammerBro -> ([Player], HammerBro, Int)
stompBro ps b
    | broDead b = (ps, b, 0)
    | otherwise = case firstPlayer (`isStomp` broPos b) (broHitbox b) ps of
        Nothing -> (ps, b, 0)
        Just i  -> (updateAt i bounce ps, b { broDead = True, broWindup = Nothing, broVel = Vec2 0 300 }, 200)

hitBoss :: [Player] -> Boss -> ([Player], Boss, Int)
hitBoss ps b
    | bossHealth b <= 0 || bossInvisible b > 0 = (ps, b, 0)
    | otherwise = case firstPlayer (`isStomp` bossPos b) (bossHitbox b) ps of
        Nothing -> (ps, b, 0)
        Just i  -> (updateAt i bounce ps, hurt, if health <= 0 then 1300 else 300)
    where
        health = bossHealth b - 1
        hurt = b { bossHealth = health, bossInvisible = 0.8,
                   bossCooldown = max (bossCooldown b) 1.2,
                   bossVolleys = 0 }

bounce :: Player -> Player
bounce p = p { playerVel = Vec2 (x (playerVel p)) 350, onGround = False }

-- | Lose a heart and do the death hop
killPlayer :: Player -> Player
killPlayer p
    | playerDead p || playerHealth p <= 0 = p
    | otherwise = p { playerDead = True, playerHealth = playerHealth p - 1,
                      playerVel = Vec2 0 450, onGround = False }

hurtBy :: [Hitbox] -> Player -> Player
hurtBy dangers p
    | not (playerDead p) && any (overlaps (playerHitbox p)) dangers = killPlayer p
    | otherwise = p

-- ===================== Random level =====================

randomLevel :: Float -> GameState -> GameState
randomLevel dt gs
    | lvlId (gsLevel gs) /= RandomVertical = gs
    | otherwise = rise dt (keepBelowRoof gs)

keepBelowRoof :: GameState -> GameState
keepBelowRoof gs = gs { gsPlayers = map stop (gsPlayers gs) }
    where
        roof = gsCamY gs + halfH
        stop p
            | playerDead p || hbTop (playerHitbox p) <= roof = p
            | otherwise = p { playerPos = Vec2 (x (playerPos p)) (roof - playerH p / 2),
                              playerVel = Vec2 (x (playerVel p)) (min 0 (y (playerVel p))) }

-- The camera starts rising once a player performs a jump and then keeps going for riseDuration
rise :: Float -> GameState -> GameState
rise dt gs = case gsRise gs of
    Nothing
        | any jumping (gsPlayers gs) -> gs { gsRise = Just 0 }
        | otherwise -> gs
    Just t
        | t >= riseDuration -> gs
        | t + dt >= riseDuration -> addFinish gs { gsRise = Just riseDuration, gsCamY = riseSpeed * riseDuration }
        | otherwise -> gs { gsRise = Just (t + dt), gsCamY = gsCamY gs + riseSpeed * dt }
    where
        jumping p = not (playerDead p) && y (playerVel p) > 0
        addFinish g = g { gsLevel = (gsLevel g) { lvlPlatforms = lvlPlatforms (gsLevel g) ++ [finishPlatform] } }

finishShown :: GameState -> Bool
finishShown gs = gsRise gs == Just riseDuration

fallLimits :: GameState -> (Float, Player -> Vec2)
fallLimits gs
    | lvlId (gsLevel gs) /= RandomVertical = (removeBelowY, playerSpawn)
    | otherwise = (camY - halfH - 60, const topSpot)
    where
        camY = gsCamY gs
        nearTop = [ h | h <- map staticHitbox (lvlPlatforms (gsLevel gs)), hbTop h < camY + 220, hbTop h > camY - 250 ]
        topSpot = case nearTop of
            [] -> Vec2 0 (camY + 200)
            hs -> let h = maximumBy (comparing hbTop) hs in Vec2 (x (hbCenter h)) (hbTop h + 60)

-- ========== Switches ==============

-- | The switch player n is standing at, if any.
findSwitch :: Int -> GameState -> Maybe SwitchType
findSwitch n gs = case drop n (gsPlayers gs) of
    p : _ | not (playerDead p) -> switchType <$> find (overlaps (playerHitbox p) . switchBox) (lvlSwitches (gsLevel gs))
    _ -> Nothing
    where
        switchBox s = Hitbox (switchPos s) 48 64
