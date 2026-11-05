module Physics ( 
    stepWorld,
    findSwitch)
    where

import Data.List (find, foldl')

import AI(
    stepEnemies,
    gravity,
    maxFallSpeed,
    removeBelowY)
    
import GameData (
    Boss (..),
    GameState (..),
    GameStatus (..),
    Goomba (..),
    HammerBro (..),
    GoombaState (..),
    Hitbox (..),
    InputHandler (..),
    Level (..),
    Player (..),
    Vec2 (..),
    Switch (..),
    SwitchType (..),
    bossHitbox,
    overlaps,
    playerHitbox,
    staticHitbox,
    goombaHitbox,
    broHitbox,
    hammerHitbox,
    defaultPlayer)

-- | One frame: players, goombas, then player/goomba contact.
stepWorld :: Float -> GameState -> GameState
stepWorld dt gs 
    | gsPaused gs || gsStatus gs /= InProgress = gs
    | otherwise = foldl' (flip step) gs (replicate i stepSize)
    where 
        limit = max 0 (min dt 0.1)
        i = max 1 (ceiling (limit / (1 / 120)) :: Int)
        stepSize = limit / fromIntegral i 

step :: Float -> GameState -> GameState
step dt gs
    | gsStatus gs /= InProgress = gs
    | otherwise = aiGs {
        gsPlayers = pAfterAttack,
        gsGoombas = goombas,
        gsBros = bros,
        gsBoss = boss,
        gsScore = gsScore gs + ptGoombas + ptBros + ptBoss,
        gsStatus = state}
    where        
        solids = map staticHitbox (lvlStatics (gsLevel gs))
        movedPlayers = map (stepPlayer dt solids) (gsPlayers gs)
        aiGs = stepEnemies dt solids (gs { gsPlayers = movedPlayers })
        (pAfterGoombas, goombas, ptGoombas) = hitGoomba movedPlayers (gsGoombas aiGs)
        (pAfterBros, bros, ptBros) = stompBros pAfterBros (gsBros aiGs)
        (pAfterBoss, boss, ptBoss) = hitBoss pAfterBoss (gsBoss aiGs)
        hamDanger = map hammerHitbox (gsHammers aiGs)
        broDanger = [broHitbox bro | bro <- bros, not (broDead bro)]
        bossDanger = case boss of 
            Just boss | bossHealth boss > 0 && bossInvisible boss <= 0 -> [bossHitbox boss]
            _ -> []
        pAfterAttack = map (hurtBy (hamDanger ++ broDanger ++ bossDanger)) pAfterBoss
        atGoal = any (\player -> not (playerDead player) &&
                     any (overlaps (playerHitbox player) . goalHitbox)
                         (lvlGoal (gsLevel gs))) pAfterAttack
        defeatBoss = case boss of 
            Just boss -> bossHealth boss <= 0
            Nothing -> False 
        state 
            | defeatBoss || atGoal = Win
            | otherwise = InProgress

goalHitbox :: Vec2 -> Hitbox
goalHitbox point = Hitbox point 32 70 -- Adjust if necessary

-- | Move player along x- and y-axis, or play deathsprite
stepPlayer :: Float -> [Hitbox] -> Player -> Player
stepPlayer dt solids pl0
    | playerDead pl0 = respawn (fallDown pl0)
    | otherwise      = respawn (anim dt (moveY (moveX (updateVel dt pl0))))
    where
        hits pl = find (overlaps (playerHitbox pl)) solids 
        
        respawn pl 
            | y (playerPos pl) < removeBelowY = defaultPlayer {
                playerPos = playerSpawn pl,
                playerSpawn = playerSpawn pl,
                playerIn = playerIn pl}
            | otherwise = pl
        
        fallDown pl = pl {
            playerPos = Vec2 (px + vx * dt) (py + vy' * dt),
            playerVel = Vec2 vx vy' }
            where
                Vec2 px py = playerPos pl 
                Vec2 vx vy = playerVel pl
                vy' = vy - gravity * dt 
        
        moveX pl =
            let Vec2 px py = playerPos pl
                Vec2 vx vy = playerVel pl
                pl1 = pl { playerPos = Vec2 (px + vx * dt) py }
            in case hits pl1 of 
                Nothing -> pl1 
                Just box -> 
                    let corX 
                        | vx > 0 = x (hbCenter box) - hbWidth box / 2 - playerW pl / 2
                        | vx < 0 = x (hbCenter box) + hbWidth box / 2 + playerW pl / 2
                        | otherwise = px 
                    in pl { 
                        playerPos = Vec2 corX py, 
                        playerVel = Vec2 0 vy }
                    
        moveY pl =
            let Vec2 px py = playerPos pl
                Vec2 vx vy = playerVel pl
                pl1 = pl { playerPos = Vec2 px (py + vy * dt) }
            in case hits pl1 of 
                Nothing -> pl1 
                Just box -> 
                    let corY 
                        | vy > 0 = y (hbCenter box) + hbHeight box / 2 + playerH pl / 2
                        | vy < 0 = y (hbCenter box) - hbHeight box / 2 - playerH pl / 2
                        | otherwise = py
                    in pl { 
                        playerPos = Vec2 corX py corY, 
                        playerVel = Vec2 0 vy,
                        onGround = vy < 0 }

updateVel :: Float -> Player -> Player
updateVel dt p = p {playerVel = Vec2 vx4 vy2}
    where
        input = playerIn p
        Vec2 vx0 vy0 = playerVel p
        dir = fromIntegral (fromEnum(inRight input) - fromEnum (inLeft input)
        vx1 = vx0 + dir * accel * dt
        vx2 
            | dir == 0 && onGround p = signum vx1 * max 0 (abs vx1 - friction * dt)
            | otherwise = vx1
        vx4 = max (-maxSpeed) (min maxSpeed vx2)
        vy1 = if inJump input && onGround then jumpSpeed else vy0            
        vy2 = max (-maxFallSpeed) (vy1 - gravity * dt)
           
-- Constants in addition to those from AI
accel, maxSpeed, friction, jumpSpeed :: Float
accel = 1500
maxSpeed = 300
friction = 1800
jumpSpeed = 550

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

stompBounce, deathHop :: Float 
stompBounce = 350    -- player bounce after a stomp
deathHop    = 450    -- player's upward hop when killed

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
    | playerDead p = p
    | any (overlaps (playerHitbox p)) dangers = killPlayer p
    | otherwise = p

isStomp :: Player -> Vec2 -> Bool
isStomp p m = 
    y (playerVel p) < 0 && 
    y (playerPos p) - playerH p / 2 > y m 

-- Each walking goomba checks the players in order; first touching player wins.
hitGoombas :: [Player] -> [Goomba] -> ([Player], [Goomba], Int)
hitGoombas ps []         = (ps, [], 0)
hitGoombas ps (gb : gbs) =
    let (ps1, gb1, pt)  = hitGoomba ps gb
        (ps2, gbs1, pts) = resolveHits ps1 gbs
    in (ps2, gb1 : gbs1)

hitGoomba :: [Player] -> Goomba -> ([Player], Goomba, Int)
hitGoomba [] gb = ([], gb, 0)
hitGoomba (p : ps) gb
    | gState gb /= Walking = (p : ps, gb)
    | playerDead p || not (overlaps (playerHitbox p) (goombaHitbox gb)) =
        let (ps', new, pt) = hitGoomba ps gb 
        in (p : ps', new, pt)
    | is Stomp p (gPos gb) = (bounce p : ps, gb { gState = Flat flatTime, gVel = Vec2 0 0 })
    | otherwise = (killPlayer p : ps, gb, 0)
    
-- Each living bro checks the players in order; the first one stomping him wins.
-- (Touching him any other way is handled by hurtBy.)
stompBros :: [Player] -> [HammerBro] -> ([Player], [HammerBro], Int)
stompBros ps []       = (ps, [], 0)
stompBros ps (b : bs) =
    let (ps1, b1, pt)  = stompBro ps b
        (ps2, bs1, pts) = stompBros ps1 bs
    in (ps2, b1 : bs1, pt + pts)

stompBro :: [Player] -> HammerBro -> ([Player], HammerBro, Int)
stompBro [] b = ([], b, 0)
stompBro (p : ps) b
    | broDead b = (p : ps, b, 0)
    | not (playerDead p) && overlaps (playerHitbox p) (broHitbox b)  && isStomp p (broPos b) = (
        bounce p : ps, 
        b { broDead = True, broWindup = Nothing, broVel = Vec2 0 broDeathHop} 200 )
    | otherwise =
        let (ps', new, pt) = stompBro ps b
        in (p : ps', new, pt)
        
-- | Every valid stop: boss loses lifepoint
hitBoss :: [Player] -> Maybe Boss -> ([Player], Maybe Boss, Int)
hitBoss ps Nothing = (ps, Nothing, 0)
hitBoss ps (Just b)
    | bossHealth b <= 0 = (ps, Just b, 0)
    | bossInvisible b > 0 = (ps, Just b, 0)
    | otherwise = case findP ps b of 
        Nothing -> (ps, Just b, 0)
        Just index -> 
            let l = bossHealth b - 1
                b' = b {
                    bossHealth = l,
                    bossPhase = if l <= 1 then 2 else 1,
                    bossInvisible = 0.8}
                r = zipWith (\i p -> if i == index then bounce p else p) [ 0 :: Int ..] ps
            in (r, Just b', if l <= 0 then 1300 else 300)     
            
findP :: [Player] -> Boss -> Maybe Int
findP ps b = case c of 
    [] -> Nothing
    i : _ -> Just i 
    where 
        p = 
            [ i
            | (i, p) <- zip [0 :: Int ..] ps
            , not (playerDead p)
            , overlaps (playerHitbox p) (bossHitbox b)
            , isStomp p (bossPos b)]

findSwitch :: Int -> GameState -> Maybe SwitchType
findSwitch n gs = case drop n (gsPlayers gs) of 
    [] -> Nothing
    p : _ 
        | playerDead p -> Nothing
        | otherwise -> case filter (avail p) (lvlSwitch (gsLevel gs)) of
            [] -> Nothing
            s : _ -> Just (switchType s)
    where 
        avail p s = overlaps (playerHitbox p) (Hitbox switchPos s) 48 64) -- Adjust if necessary
            