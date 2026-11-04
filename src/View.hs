module View (drawApp) where

import Graphics.Gloss

import GameData
  (AppState (..), GameState (..), Goomba (..), GoombaState (..), Hammer (..), HammerBro (..), Hitbox (..), Level (..), LevelId (..), Player (..), Screen (..), SpriteAtlas
  , StaticObj (..), Vec2 (..), broHitbox, goombaHitbox, hammerHitbox, levelName, playerHitbox)
  
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
    , pictures (map (drawGoomba atlas (gsDebug gs)) (gsGoombas gs))
    , pictures (map (drawBro atlas (gsDebug gs)) (gsBros gs))
    , pictures (map (drawHammer atlas (gsDebug gs)) (gsHammers gs))
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
  | playerDead p = [ch, 'D']
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

goombaSprite :: Goomba -> String
goombaSprite gb = case gState gb of
  Walking -> "GW" ++ show ((floor (gPhase gb) :: Int) `mod` 3)
  Flat _  -> "GF"
  Dying   -> "GD"

drawGoomba :: SpriteAtlas -> Bool -> Goomba -> Picture
drawGoomba atlas debug gb =
  translate (x c) (y c) $
    if debug
      then color silverWire $ rectangleWire (hbWidth hb) (hbHeight hb)
      else case lookup (goombaSprite gb) atlas of
             Just pic -> scale 2 2 pic
             Nothing  -> color (makeColor 0.6 0.3 0.1 1.0) $ rectangleSolid (hbWidth hb) (hbHeight hb)
  where
    c  = gPos gb
    hb = goombaHitbox gb

-- Idle: HI0/HI1 swapping every 0.4 s. Throw pose: HT, which is 32x39 instead
-- of 24x32, so shift it (+8, +7) to keep the feet in the same place.
drawBro :: SpriteAtlas -> Bool -> HammerBro -> Picture
drawBro atlas debug b
  | debug     = translate (x c) (y c) $ color silverWire $ rectangleWire (hbWidth hb) (hbHeight hb)
  | otherwise = translate (x c + dx) (y c + dy) $ case lookup (if broDead b then "HI0" else name) atlas of
      Just pic -> scale 2 (2 * flipY) pic
      Nothing  -> color (makeColor 0.2 0.7 0.3 1.0) $ rectangleSolid (hbWidth hb) (hbHeight hb)
  where
    c  = broPos b
    hb = broHitbox b
    (name, dx, dy) = case broWindup b of
      Just _  -> ("HT", 8, 7)
      Nothing -> ("HI" ++ show ((floor (broAnim b / 0.4) :: Int) `mod` 2), 0, 0)
    -- dead: HI0 drawn upside down (no separate death sprite)
    flipY = if broDead b then -1 else 1

-- Spins through H0..H6 at 14 frames per second.
drawHammer :: SpriteAtlas -> Bool -> Hammer -> Picture
drawHammer atlas debug h =
  translate (x c) (y c) $
    if debug
      then color silverWire $ rectangleWire (hbWidth hb) (hbHeight hb)
      else case lookup ("H" ++ show ((floor (hamAnim h * 14) :: Int) `mod` 7)) atlas of
             Just pic -> scale 2 2 pic
             Nothing  -> color white $ rectangleSolid (hbWidth hb) (hbHeight hb)
  where
    c  = hamPos h
    hb = hammerHitbox h

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
