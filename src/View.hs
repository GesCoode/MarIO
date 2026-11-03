module View (drawApp) where

import Graphics.Gloss

import GameData
  (AppState (..), GameState (..), Hitbox (..), Level (..), LevelId (..), Player (..), Screen (..), SpriteAtlas
  , StaticObj (..), Vec2 (..), levelName, playerHitbox)
  
import Menu (menuButtons)

grayButton :: Color
grayButton = makeColor 0.45 0.45 0.45 1.0

yellowButton :: Color
yellowButton = makeColor 1.0 0.85 0.0 1.0

-- Bright silver for wireframe-only debug view.
silverWire :: Color
silverWire = makeColor 0.92 0.92 0.96 1.0

drawApp :: AppState -> IO Picture
drawApp app = pure $ case screen app of
  Menu       -> drawMenu
  Playing gs -> drawPlaying (sprites app) gs

drawMenu :: Picture
drawMenu =
  pictures
    [ color black (rectangleSolid 800 600)
    , pictures (map drawButton menuButtons)
    , cornerLabel "Menu"
    ]

drawPlaying :: SpriteAtlas -> GameState -> Picture
drawPlaying atlas gs =
  pictures
    [ color black (rectangleSolid 800 600)
    , pictures (map (drawStatic atlas (gsDebug gs)) (lvlStatics (gsLevel gs)))
    , pictures (zipWith (drawPlayer atlas (gsDebug gs)) playerChars (gsPlayers gs))
    , cornerLabel (levelName (lvlId (gsLevel gs)))
    ]

drawStatic :: SpriteAtlas -> Bool -> StaticObj -> Picture
drawStatic atlas debug s =
  translate (x c) (y c) $
    if debug
      then color silverWire (rectangleWire (soWidth s) (soHeight s))
      else body
  where
    c = soCenter s
    body = case lookup (soSprite s) atlas of
      Just pic -> pic
      Nothing  -> color white $ rectangleSolid (soWidth s) (soHeight s)

-- Player 0 is Mario, player 1 is Luigi (sprite name prefix).
playerChars :: [Char]
playerChars = "ML"

-- Sprite name: "MJR" (jump), "ML2" (walk frame), ...
playerSprite :: Char -> Player -> String
playerSprite ch p
  | onGround p = [ch, dir] ++ show frame
  | otherwise  = [ch, 'J', dir]
  where
    dir   = if facingRight p then 'R' else 'L'
    frame = (floor (walkPhase p) :: Int) `mod` 3

drawPlayer :: SpriteAtlas -> Bool -> Char -> Player -> Picture
drawPlayer atlas debug ch p =
  translate (x c) (y c) $
    if debug
      then color silverWire $ rectangleWire (hbWidth hb) (hbHeight hb)
      else case lookup (playerSprite ch p) atlas of
             Just pic -> scale 2 2 pic
             Nothing  -> color white $ rectangleSolid (playerW p) (playerH p)
  where
    c  = playerPos p
    hb = playerHitbox p

drawButton :: (LevelId, Float) -> Picture
drawButton (lid, cy) =
  translate 0 cy $ color (buttonColor lid) $
    rectangleSolid 400 70

buttonColor :: LevelId -> Color
buttonColor Sandbox = yellowButton
buttonColor _       = grayButton

cornerLabel :: String -> Picture
cornerLabel label =
  translate (-380) 270 $ scale 0.12 0.12 $ color white $ text label
