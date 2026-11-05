module Procedural (
    createVert,
    nextSeed, 
    checkAvail)
    where
    
import GameData (
    GoombaStart (..),
    Level (..),
    LevelId (..),
    PlayerStart (..),
    StaticObj (..),
    Switch (..),
    SwitchType (..),
    Vec2 (..))
    
-- Ref: https://www.gnu.org/software/gsl/doc/html/rng.html#unix-random-number-generators
nextSeed :: Int -> Int
nextSeed n = (n * 1103515245 + 12345) `mod` 2147483648

-- | Vertical platforms
createVert :: Int -> Level
createVert seed = Level {
    lvlId = RandomVertical,
    lvlStatics = ground : platforms,
    lvlPlayers = [
        PlayerStart (Vec2 (-300) (-215)) (Vec2 0 0),
        PlayerStart (Vec2 (-250) (-215)) (Vec2 0 0)],
    lvlGoombas = [GoombaStart (Vec2 firstX (-142)) 1],
    lvlBros = [],
    lvlGoals = [Vec2 finalX 211],
    lvlBosses = [],
    lvlSwitches = [Switch (Vec2 lastX 243) spot]}
    where 
        ground = StaticObj (Vec2 0 (-275)) 790 60 "block"
        heights = [-170, -95, -20, 55, 130, 205]
        widths = take (length heights) (iterate nextSeed seed)
        positions = scanl shifts initX widths
        initX = -185 + fromIntegral (seed `mod` 51) - 25
        shifts xPos newSeed = max (-175) (min 175 (xPos + fromIntegral (newSeed `mod` 141) - 70))
        xValues = take (length heights) (tail positions)
        platforms = zipWith createStatic xValues heights
        createStatic xPos height = StaticObj (Vec2 xPos height) 185 18 "block"
        firstX = case xValues of 
            [] -> initX
            xPos : _ -> xPos
        lastX = fold (\_ xPos -> xPos) initX xValues
        
-- | Check measurements 
checkAvail :: Level -> Bool
checkAvail lvl = 
    not (null (lvlStatics lvl)) &&
    all inFrame platforms &&
    and (zipWith inRangeJump (ground : platforms) platforms)
    where 
        ground = case lvlStatics lvl of 
            [] -> StaticObj (Vec2 0 (-275)) 790 60 "block"
            first : _ -> first
        platforms = case lvlStatics lvl of
            [] -> []
            _ : rest -> rest
        inFrame frame =
            abs (x (hbCenter frame)) + hbWidth frame / 2 <= 400 &&
            abs (y (hbCenter frame)) + hbHeight frame / 2 <= 300
        inRangeJump from to =
            abs (x (hbCenter to) - x(hbWidth from)) <= 120 &&
            abs (y (hbCenter to) - y(hbHeight from)) <= 260