module AI (stepEnemies) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import GameData ( 
    GameState (..),
    Level (..),
    StaticObj (..),
    Vec2 (..),
    Player (..),
    Goomba (..),
    GoombaState (..),
    HammerBro (..),
    Hammer (..),
    HitBox (..),
    goombaH,
    goombaW,
    overlaps,
    staticHitbox,
    hammerHitBox)
    
-- | Update enemies 
stepEnemies :: Float -> GameState -> GameState
stepEnemies dt gs = gs {
    gsGoombas = filter visGoomba (map (stepGoomba dt pltfrms) (gsGoombas gs)),
    gsBros = filter visBro newBros,
    gsHammers = filter (visHammer pltfrms) (map stepHammer dt) (gsHammers gs) ++ newHammers }
    where 
        pltfrms = lvlStatics (gsLevel gs)
        (newBros, newHammers) = procBros dt (gsPlayers gs) (gsBros gs)
        
-- | Goomba walks, falls, gets squished or disappears after death
stepGoomba :: Float -> [StaticObj] -> Goomba -> Goomba
stepGoomba dt plftms enemy = case gState enemy of 
    Walking -> moveGoomba dt pltfrms enemy
    Flat rest 
        | rest > dt -> enemy { gState = Flat (rest - dt) }
        | otherwise -> enemy { gState = Dying, gVel = Vec2 (gDir enemy * 130) 280 }
    Dying -> stepDyingGoomba dt enemy

-- | Move dead Goomba
stepDyingGoomba :: Float -> Goomba -> Goomba
stepDyingGoomba dt enemy = enemy { 
    gPos = Vec2 (posX + velX * dt) (posY + newY * dt),
    gVel = Vec2 velX newY }
    where
        Vec2 posX posY = gPos enemy
        Vec2 velX velY = gVel enemy
        newY = velY - goombaGrav * dt 

-- | Turn when edge or wall
moveGoomba :: Float -> [StaticObj] -> Goomba -> Goomba
moveGoomba dt pltfrms enemy = enemy {
    gPos = Vec2 dX dY,
    gVel = Vec2 0 tY,
    gDir = direction, 
    gPhase = gPhase enemy + dt * 7}
    where 
        Vec2 posX posY = gPos enemy
        Vec2 _ velY = gVel enemy
        groundSurface = Hitbox (Vec2 posX (posY - goombaH / 2 - 2)) 4 8
        onGround = collide pltfrms groundSurface
        forwardX = posX + gDir enemy * (goombaW / 2 + 12)
        surfaceAhead = Hitbox (Vec2 forwardX (posY - goombaH / 2 - 2)) 4 8
        noGround = onGround && not (collide pltfrms surfaceAhead)
        testX = posY + gDir enemy * goombaSpeed * dt
        hitWall = collide pltfrms (Hitbox (Vec2 testX posY) goombaW goombaH)
        direction 
            | hitWall || noGround = negate (gDir enemy)
            | otherwise = gDir enemy 
        movedX
            | hitWall || noGround = posX
            | otherwise = testX
        fallY = max(-700) (velY - goombaGrav * dt)
        testY = posY + fallY * dt
        hitGround = collide pltfrms (Hitbox (Vec2 movedX testY) goombaW goombaH)
        movedY 
            | hitGround = posY
            | otherwise = testY
        finalY
            | hitGround = 0
            | otherwise fallY
            
goombaSpeed, goombaGrav :: Float
goombaSpeed = 75
goombaGrav = 1000

procBros :: Float -> [Player] -> [HammerBro] -> ([HammerBro], [Hammer])
procBros dt players bros = (map fst res, concatMap snd res)
    where 
        res = map (stepBro dt players) bros

stepBro :: Float -> [Player] -> HammerBro -> (HammerBro, [Hammer])
-- STILL TO DO 

throwHammer :: [Player] -> HammerBro -> [Hammer]
-- STILL TO DO 

createHammer :: HammerBro -> Player -> Hammer 
-- STILL TO DO 

nearestPlayer :: Vec2 -> [Player] -> MaybePLayer
-- STILL TO DO 

distQuad :: Vec2 -> Vec2 -> Float
distQuad a b = (x a - x b) ^ (2 :: Int) + (y a - y b) ^ (2 :: Int)

stepHammer :: Float -> Hammer -> Hammer 
-- STILL TO DO 

stepDeadBro :: Float -> HammerBro -> HammerBro
-- STILL TO DO 

visGoomba :: Goomba -> Bool
visGoomba enemy = y (gPos enemy) > -400 -- Adjust if necessary

visBro :: HammerBro -> Bool
visBro enemy = not (broDead enemy) || y (broPos enemy) > -400 -- Adjust if necessary

visHammer :: [StaticObj] -> Hammer -> Bool
visHammer pltfrms hammer = 
    abs (x (hamPos hammer)) < 400 && abs (y (hamPos hammer)) < 400 && -- Adjust if necessary
    not (collide pltfrms (hammerHitBox hammer))

collide :: [StaticObj] -> Hitbox -> Bool
collide pltfrms hb = any (overlaps hb . staticHitbox) pltfrms

atckInterval, hammerGrav :: Float
atckInterval = 2 -- Adjust if necessary
hammerGrav = 800 -- Adjust if necessary