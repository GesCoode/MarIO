module Procedural (
    createVert,
    nextSeed, 
    checkAvail)
    where
    
import Data.List (sortOn)

import GameData (
    Level (..),
    LevelId (..),
    PlayerStart (..),
    StaticObj (..),
    SwitchType (..),
    Vec2 (..),
    finishTop)
    
-- Ref: https://www.gnu.org/software/gsl/doc/html/rng.html#unix-random-number-generators
nextSeed :: Int -> Int
nextSeed n = (n * 1103515245 + 12345) `mod` 2147483648

-- | Random level: a ground between two walls that reach far up, and a climb of
--   one-way platforms up to just below the finish. The camera, the finish platform
--   and dying below the screen are handled in Physics.
--   The exit is not used: winning this level already continues the campaign.
createVert :: Int -> SwitchType -> Level
createVert seed _ = Level {
    lvlId = RandomVertical,
    lvlStatics = [ground, wall (-1), wall 1],
    lvlPlayers = [
        PlayerStart (Vec2 (-60) (-215)) (Vec2 0 0),
        PlayerStart (Vec2 60 (-215)) (Vec2 0 0)],
    lvlGoombas = [],
    lvlBros = [],
    lvlGoals = [],
    lvlBosses = [],
    lvlSwitches = [],
    lvlPlatforms = climb seed}
    where 
        ground = StaticObj (Vec2 0 (-268)) (2 * innerX) 64 "Ground"
        wall side = StaticObj (Vec2 (side * (innerX + 16)) (wallH / 2 - 400)) 32 wallH "Square"
        wallH = finishTop + 1000

-- | Inner edge of the walls (the screen is 800 wide, walls are 32 wide).
innerX :: Float
innerX = 368

platformH :: Float
platformH = 16   -- texture is 16x8, drawn at 2x

-- | Platforms from just above the ground to just below the finish. Each one is
--   75-110 px above the previous and at most 160 px to the side, so the next one
--   is always in jumping range (a jump reaches about 164 px). Sometimes an extra
--   platform appears on the other side.
climb :: Int -> [StaticObj]
climb = go (-236) 0
    where
        go topPrev xPrev s
            | topNew > finishTop - 40 = []
            | otherwise = platform xNew topNew w : extra ++ go topNew xNew s4
            where
                s1 = nextSeed s
                s2 = nextSeed s1
                s3 = nextSeed s2
                s4 = nextSeed s3
                topNew = topPrev + 75 + fromIntegral (s1 `mod` 36)
                w = [96, 128, 160] !! (s2 `mod` 3)
                xNew = inside w (xPrev + fromIntegral (s3 `mod` 321) - 160)
                extra = [ platform (inside 96 (negate xNew)) topNew 96 | s4 `mod` 3 == 0, abs xNew > 150 ]
        platform cx top w = StaticObj (Vec2 cx (top - platformH / 2)) w platformH "Platform"
        inside w cx = max (-innerX + w / 2 + 8) (min (innerX - w / 2 - 8) cx)

-- | Check that the platforms stay between the walls and that no gap upwards is
--   higher than a jump.
checkAvail :: Level -> Bool
checkAvail lvl = 
    not (null tops) &&
    all between (lvlPlatforms lvl) &&
    all (<= 130) (zipWith (-) tops ((-236) : tops))
    where 
        tops = sortOn id [y (soCenter p) + soHeight p / 2 | p <- lvlPlatforms lvl]
        between p = abs (x (soCenter p)) + soWidth p / 2 <= innerX
