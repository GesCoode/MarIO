module Procedural (
    createVert,
    finishPlatform,
    nextSeed, 
    checkAvail)
    where
    
import Data.List (sort, sortOn)

import GameData (
    GoombaStart (..),
    Level (..),
    LevelId (..),
    PlayerStart (..),
    StaticObj (..),
    Vec2 (..),
    finishTop,
    soTop)
    
-- Ref: https://www.gnu.org/software/gsl/doc/html/rng.html#unix-random-number-generators
nextSeed :: Int -> Int
nextSeed n = (n * 1103515245 + 12345) `mod` 2147483648

createVert :: Int -> Level
createVert seed = Level {
    lvlId = RandomVertical,
    lvlStatics = [ground, wall (-1), wall 1],
    lvlPlayers = [
        PlayerStart (Vec2 (-60) (-215)) (Vec2 0 0),
        PlayerStart (Vec2 60 (-215)) (Vec2 0 0)],
    lvlGoombas = goombasOn platforms,
    lvlBros = [],
    lvlGoals = [],
    lvlBosses = [],
    lvlSwitches = [],
    lvlPlatforms = platforms}
    where 
        platforms = climb seed
        ground = StaticObj (Vec2 0 (groundTop - 32)) (2 * innerX) 64 "Ground"
        wall side = StaticObj (Vec2 (side * (innerX + 16)) (wallH / 2 - 400)) 32 wallH "Square"
        wallH = finishTop + 1000

innerX :: Float
innerX = 368

groundTop :: Float
groundTop = -236

finishPlatform :: StaticObj
finishPlatform = StaticObj (Vec2 0 (finishTop - platformH / 2)) (2 * innerX) platformH "Platform"

platformH :: Float
platformH = 16 

climb :: Int -> [StaticObj]
climb = go groundTop 0
    where
        go topPrev xPrev s
            | topNew > finishTop - 40 = []
            | otherwise = platform xNew topNew w : extra ++ go topNew xNew (roll 4)
            where
                roll n = iterate nextSeed s !! n   -- the n-th random number after s
                topNew = topPrev + 75 + fromIntegral (roll 1 `mod` 36)
                w = [96, 128, 160] !! (roll 2 `mod` 3)
                xNew = inside w (xPrev + fromIntegral (roll 3 `mod` 321) - 160)
                extra = [ platform (inside 96 (negate xNew)) topNew 96 | roll 4 `mod` 3 == 0, abs xNew > 150 ]
        platform cx top w = StaticObj (Vec2 cx (top - platformH / 2)) w platformH "Platform"
        inside w cx = max (-innerX + w / 2 + 8) (min (innerX - w / 2 - 8) cx)


goombasOn :: [StaticObj] -> [GoombaStart]
goombasOn plats =
    [ GoombaStart (Vec2 (x (soCenter p)) (soTop p + 15)) (if even i then 1 else -1)
    | (i, p) <- zip [0 :: Int ..] (sortOn soTop plats)
    , i >= 3, i `mod` 3 == 0, soWidth p >= 128 ]


checkAvail :: Level -> Bool
checkAvail lvl = 
    not (null tops) &&
    all between (lvlPlatforms lvl) &&
    all (<= 130) (zipWith (-) tops (groundTop : tops))
    where 
        tops = sort (map soTop (lvlPlatforms lvl))
        between p = abs (x (soCenter p)) + soWidth p / 2 <= innerX
