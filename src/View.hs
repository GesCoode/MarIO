module View (drawApp) where

import Graphics.Gloss
import GameData (AppState (..),  Boss (..),
    GameState (..),
    Goomba (..), 
    GoombaState (..), 
    Hammer (..), 
    HammerBro (..), 
    Highscore (..),
    Hitbox (..), 
    Level (..), 
    LevelId (..), 
    Player (..), 
    PlayerSelect (..),
    Screen (..), 
    SpriteAtlas,
    StaticObj (..), 
    Vec2 (..), 
    bossHitbox,
    broHitbox, 
    goombaHitbox, 
    hammerHitbox, 
    levelName, 
    maxHealth,
    playerHitbox)

import Physics (goalHitbox)
import Menu (
    MenuSelect (..),
    menuButtons,
    menuW,
    menuH,
    playerBtn,
    playerBtnW,
    playerBtnH,
    playerBtnY)

grayButton :: Color
grayButton = makeColor 0.45 0.45 0.45 1.0

yellowButton :: Color
yellowButton = makeColor 1.0 0.85 0.0 1.0

-- Bright silver for wireframe-only debug view.
silverWire :: Color
silverWire = makeColor 0.92 0.92 0.96 1.0

drawApp :: AppState -> IO Picture
drawApp app = pure $ case screen app of
    Menu       -> drawMenu (playerSelect app)
    Playing gs -> drawPlaying (sprites app) gs
    Result won score -> drawResult won score (highscores app)

-- | Show level select and player select
drawMenu :: PlayerSelect -> Picture
drawMenu select =
    pictures [ 
        color black (rectangleSolid 800 600),
        pictures (map drawButton menuButtons),
        pictures (map (drawPlayerButton select) playerBtn),
        cornerLabel "Menu" ]

drawPlayerButton :: PlayerSelect -> (PlayerSelect, Float) -> Picture
drawPlayerButton select (opt, mx) =
    translate mx playerBtnY $
        pictures [
            color background (rectangleSolid playerBtnW playerBtnH),
            translate (-45) (-5) $
                scale 0.14 0.14 $
                    color textColour (text name)]
    where
        chosen = select == opt
        background = if chosen then yellowButton else grayButton
        textColour = if chosen then black else white
        name = case opt of 
            SinglePlayer -> "Single Player"  
            TwoPlayer    -> "Two Players"            
                  
drawPlaying :: SpriteAtlas -> GameState -> Picture
drawPlaying atlas gs =
    pictures [ 
        color black (rectangleSolid 800 600),
        drawBackground atlas gs,
        -- everything in the world moves with the camera; the label stays put
        translate (negate (cameraX gs)) (negate (gsCamY gs)) $ pictures [
            pictures (map (drawStatic atlas (gsDebug gs)) (lvlStatics (gsLevel gs))),
            pictures (map (drawStatic atlas (gsDebug gs)) (lvlPlatforms (gsLevel gs))),
            pictures (map (drawGoomba atlas (gsDebug gs)) (gsGoombas gs)),
            pictures (map (drawBro atlas (gsDebug gs)) (gsBros gs)),
            pictures (map (drawHammer atlas (gsDebug gs)) (gsHammers gs)),
            maybe blank (drawBoss atlas (gsDebug gs) (gsPlayers gs)) (gsBoss gs),
            pictures (zipWith (drawPlayer atlas (gsDebug gs)) playerChars (gsPlayers gs)),
            -- goals are invisible, except as green outlines in the debug view
            if gsDebug gs then pictures (map (drawFinish . goalHitbox) (lvlGoals (gsLevel gs))) else blank],
        cornerLabel (levelName (lvlId (gsLevel gs))),
        scoreLabel (gsScore gs),
        pictures (zipWith3 (healthRow atlas) [0 ..] ["MARIO", "LUIGI"] (gsPlayers gs))]

drawFinish :: Hitbox -> Picture
drawFinish hb =
  translate (x (hbCenter hb)) (y (hbCenter hb)) $
    color green $ rectangleWire (hbWidth hb) (hbHeight hb)

-- | One background image.
data Layer = Layer {
    lySprite   :: String,
    lySize     :: (Float, Float),   -- in pixels
    lyBottom   :: Float,            -- y of the image's bottom edge
    lyParallax :: Bool}             -- True: parallax (see drawBackground); False: moves with the level

-- | Per level: a fill colour for where no image reaches, and the images back to front.
backgroundFor :: LevelId -> Maybe (Color, [Layer])
backgroundFor Level1 = Just (skyBlue,
    [ Layer "Clouds" (4467, 83) (-300 + 64 + 416 - 166) True   -- clouds high in the sky
    , Layer "1-1-Background" (3584, 104) (-300 + 64) False ])  -- hills etc., standing on the ground
backgroundFor Level3 = Just (black, [Layer "BossFightBackground" (208, 63) (-300 - 64) True])
backgroundFor _      = Nothing

-- | #5C94FC, the sky colour of World 1-1.
skyBlue :: Color
skyBlue = makeColorI 0x5C 0x94 0xFC 255

-- | Background: fill colour first, then the layers. Scaled 2x like everything else
--   (more if needed to cover the 800 px width).
--   Parallax layer (in screen space): when the camera is at the start of the level its
--   left edge is at the left of the screen, at the end its right edge is at the right,
--   so a layer wider than the level scrolls faster than the camera.
--   Other layers are part of the level: their left edge is at the level's left edge
--   and they move exactly with the camera.
drawBackground :: SpriteAtlas -> GameState -> Picture
drawBackground atlas gs = case backgroundFor (lvlId (gsLevel gs)) of
    Nothing -> blank
    Just (fill, layers) -> pictures (color fill (rectangleSolid 800 600) : map layer layers)
    where
      (lo, hi) = cameraRange gs
      layer ly = case lookup (lySprite ly) atlas of
        Nothing  -> blank
        Just pic ->
            let (pw, ph) = lySize ly
                s  = max 2 (fromIntegral (ceiling (800 / pw) :: Int))
                bw = pw * s
                bh = ph * s
                t  = if hi > lo then (cameraX gs - lo) / (hi - lo) else 0.5
                left
                    | lyParallax ly = -400 - t * (bw - 800)          -- screen x of the left edge
                    | otherwise     = (lo - 400) - cameraX gs        -- level's left edge, on screen
            in translate (left + bw / 2) (lyBottom ly + bh / 2) (scale s s pic)

-- | Lowest and highest camera x for this level (the camera stays inside the level).
cameraRange :: GameState -> (Float, Float)
cameraRange gs
    | null statics = (0, 0)
    | right < left = let mid = (left + right) / 2 in (mid, mid)
    | otherwise    = (left, right)
    where
        statics = lvlStatics (gsLevel gs)
        left    = minimum [x (soCenter s) - soWidth s / 2 | s <- statics] + 400
        right   = maximum [x (soCenter s) + soWidth s / 2 | s <- statics] - 400

-- | Camera x: midpoint between the living players (all players if none are alive),
--   kept inside the level so the screen never shows past its left or right edge.
cameraX :: GameState -> Float
cameraX gs = clamp (midpoint (map (x . playerPos) followed))
  where
    alive    = filter (not . playerDead) (gsPlayers gs)
    followed = if null alive then gsPlayers gs else alive
    midpoint [] = 0
    midpoint xs = (minimum xs + maximum xs) / 2

    (lo, hi) = cameraRange gs
    clamp c
      | null (lvlStatics (gsLevel gs)) = c
      | otherwise = max lo (min hi c)

drawStatic :: SpriteAtlas -> Bool -> StaticObj -> Picture
drawStatic atlas debug s =
    translate (x c) (y c) $
        if debug
          then color silverWire (rectangleWire (soWidth s) (soHeight s))
          else body
    where
        c = soCenter s
        body = case lookup (soSprite s) atlas of
            Just pic -> tiled (textureSize (soSprite s)) (soWidth s) (soHeight s) pic
            Nothing  -> color white $ rectangleSolid (soWidth s) (soHeight s)

-- | Pixel size of a level texture: most are 16x16, the one-way platform is 16x8.
textureSize :: String -> (Float, Float)
textureSize "Platform" = (16, 8)
textureSize _          = (16, 16)

-- Repeat a texture (drawn at 2x) across a w x h block.
-- Tiles stretch slightly when the size is not a multiple of the tile size.
tiled :: (Float, Float) -> Float -> Float -> Picture -> Picture
tiled (texW, texH) w h pic = pictures
  [ translate (-w / 2 + tw * (fromIntegral i + 0.5)) (-h / 2 + th * (fromIntegral j + 0.5)) $
      scale (tw / texW) (th / texH) pic
  | i <- [0 .. nx - 1], j <- [0 .. ny - 1] ]
  where
    nx = max 1 (round (w / (2 * texW))) :: Int
    ny = max 1 (round (h / (2 * texH))) :: Int
    tw = w / fromIntegral nx
    th = h / fromIntegral ny

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
-- | Boss sprites (16x24, drawn at 3x):
--   BT0/BT1 alternate while he is about to throw or jump, and while jumping;
--   BTN when standing still or hurt (flickering after a stomp).
--   The sprites face left; he is flipped to face the nearest living player.
drawBoss :: SpriteAtlas -> Bool -> [Player] -> Boss -> Picture
drawBoss atlas debug players b
    | bossHealth b <= 0 = blank
    | flicker           = blank
    | debug     = translate (x c) (y c) $ color silverWire $ rectangleWire (hbWidth hb) (hbHeight hb)
    | otherwise = translate (x c) (y c) $ case lookup sprite atlas of
        Just pic -> scale (3 * facing) 3 pic
        Nothing  -> color (makeColor 0.8 0.1 0.1 1.0) $ rectangleSolid (hbWidth hb) (hbHeight hb)
    where
        c  = bossPos b
        hb = bossHitbox b
        hurt    = bossInvisible b > 0
        flicker = hurt && even (floor (bossInvisible b * 10) :: Int)
        busy    = bossJumping b || bossCooldown b < 0.4   -- jumping, or about to act
        sprite
            | hurt      = "BTN"
            | busy      = "BT" ++ show ((floor (bossAnim b * 8) :: Int) `mod` 2)
            | otherwise = "BTN"
        alive   = [x (playerPos p) | p <- players, not (playerDead p)]
        nearest = case alive of
            [] -> x c
            ps -> snd (minimum [(abs (px - x c), px) | px <- ps])
        facing  = if nearest > x c then -1 else 1

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

drawButton :: (MenuSelect, Float) -> Picture
drawButton (choice, cy) =
    translate 0 cy $ 
        pictures [ 
            color (buttonColor choice) (rectangleSolid menuW menuH),
            translate (-42) (-6) $ scale 0.14 0.14 $ color black $ text (buttonLabel choice)]

buttonLabel :: MenuSelect -> String
buttonLabel (SelectLevel lid) = levelName lid
buttonLabel StartCampaign     = "Campaign"

buttonColor :: MenuSelect -> Color
buttonColor (SelectLevel Sandbox) = yellowButton
buttonColor _                     = grayButton

-- | Game won / game over: the final score and the saved highscores.
drawResult :: Bool -> Int -> [Highscore] -> Picture
drawResult won score scores =
    pictures ([
        color black (rectangleSolid 800 600),
        centered 0.45 (if won then yellowButton else red) 190 (if won then "GAME WON" else "GAME OVER"),
        centered 0.2 white 120 ("YOUR SCORE  " ++ show score),
        centered 0.15 grayButton 60 "PREVIOUS SCORES"]
        ++ zipWith scoreLine [1 :: Int ..] (take 8 scores)
        ++ [centered 0.11 grayButton (-260) "Press any key"])
    where
        scoreLine i hs = translate (-170) (20 - 30 * fromIntegral i) $ scale 0.13 0.13 $ color white $
            text (show i ++ ".  " ++ levelName (hsLevel hs) ++ replicate (14 - length (levelName (hsLevel hs))) ' ' ++ show (hsScore hs))

-- | Text centered horizontally (Gloss characters are about 104 units wide).
centered :: Float -> Color -> Float -> String -> Picture
centered s c yPos str =
    translate (negate (fromIntegral (length str) * 104 * s / 2)) yPos $ scale s s $ color c $ text str

-- | Score (which is also the timer), top right.
-- | One row of hearts per player under the level name: full hearts for the health
--   left, empty ones for what is lost. Hearts are 15x13, drawn at 2x.
healthRow :: SpriteAtlas -> Int -> String -> Player -> Picture
healthRow atlas row name p =
    translate (-380) (232 - 34 * fromIntegral row) $ pictures (label : hearts)
    where
        label = translate 0 (-5) $ scale 0.1 0.1 $ color white $ text name
        hearts = [ translate (85 + 34 * fromIntegral i) 0 (heart (i < playerHealth p))
                 | i <- [0 .. maxHealth - 1] ]
        heart full = case lookup (if full then "FullHeart" else "EmptyHeart") atlas of
            Just pic -> scale 2 2 pic
            Nothing  -> color (if full then red else greyN 0.4) $ circleSolid 12

scoreLabel :: Int -> Picture
scoreLabel score =
    translate 230 270 $ scale 0.15 0.15 $ color white $ text ("SCORE " ++ show score)

cornerLabel :: String -> Picture
cornerLabel label =
    translate (-380) 270 $ scale 0.12 0.12 $ color white $ text label
