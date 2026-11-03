module Physics
  ( stepPlayer
  ) where

import GameData
  ( GameState (..)
  , InputHandler (..)
  , Level (..)
  , Player (..)
  , Vec2 (..)
  , overlaps
  , playerHitbox
  , staticHitbox
  )

stepPlayer :: Float -> GameState -> GameState
stepPlayer dt gs = gs { gsPlayers = map stepOne (gsPlayers gs) }
  where
    stepOne pl = updateAnim dt (moveY (moveX (updateVel dt (playerIn pl) pl)))
    solids = map staticHitbox (lvlStatics (gsLevel gs))
    hits pl = any (overlaps (playerHitbox pl)) solids

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
        
        vx2 | dir == 0 && onGround p = signum vx1 * max 0 (abs vx1 - friction * dt)
            | otherwise = vx1
        
        vx4 = max (-maxSpeed) (min maxSpeed vx2)
        
        vy1 | inJump inp && onGround p = jumpSpeed
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
    facing | vx >  1   = True
           | vx < -1   = False
           | otherwise = facingRight p
    phase | not (onGround p) = walkPhase p
          | abs vx < 1       = 0
          | otherwise        = walkPhase p + abs vx * dt / stepLength
