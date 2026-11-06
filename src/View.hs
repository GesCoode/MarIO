module View (drawApp) where

import Graphics.Gloss

import GameData
import Menu (MenuSelect (..), menuButtons, menuW, menuH, playerBtn, playerBtnW, playerBtnH, playerBtnY)
import Physics (goalHitbox)

drawApp :: AppState -> IO Picture
drawApp app = pure $ case screen app of
    Menu             -> drawMenu (playerSelect app)
    Playing gs       -> drawPlaying (sprites app) gs
    Result won score -> drawResult won score (highscores app)

-- ===================== Small helpers =====================

grayButton, yellowButton, silverWire, skyBlue :: Color
grayButton   = makeColor 0.45 0.45 0.45 1.0
yellowButton = makeColor 1.0 0.85 0.0 1.0
silverWire   = makeColor 0.92 0.92 0.96 1.0   -- debug
skyBlue      = makeColorI 0x5C 0x94 0xFC 255

-- Move a picture to a point
at :: Vec2 -> Picture -> Picture
at v = translate (x v) (y v)

fullScreen :: Color -> Picture
fullScreen c = color c (rectangleSolid (2 * halfW) (2 * halfH))

label :: (Float, Float) -> Float -> Color -> String -> Picture
label (tx, ty) s c str = translate tx ty $ scale s s $ color c $ text str

centered :: Float -> Color -> Float -> String -> Picture
centered s c ty str = label (negate (fromIntegral (length str) * 104 * s / 2), ty) s c str




drawSprite :: SpriteAtlas -> Bool -> Hitbox -> String -> (Float, Float) -> Color -> Picture
drawSprite atlas debug hb name (sx, sy) fallback = at (hbCenter hb) $
    if debug then color silverWire (rectangleWire w h)
    else maybe (color fallback (rectangleSolid w h)) (scale sx sy) (lookup name atlas)
    where
        w = hbWidth hb
        h = hbHeight hb

normal :: (Float, Float)
normal = (pixelScale, pixelScale)

-- Frame number for an animation { time (or distance) * speed }
frame :: Float -> Int -> String
frame t n = show ((floor t :: Int) `mod` n)

-- =============== Menu =====================

drawMenu :: PlayerSelect -> Picture
drawMenu select = pictures (
    fullScreen black
    : map drawButton menuButtons
    ++ map (drawPlayerButton select) playerBtn
    ++ [cornerLabel "Menu"])

drawButton :: (MenuSelect, Float) -> Picture
drawButton (choice, cy) = translate 0 cy $ pictures [
    color background (rectangleSolid menuW menuH),
    label (-42, -6) 0.14 black name ]
    where
        (name, background) = case choice of
            SelectLevel Sandbox -> (levelName Sandbox, yellowButton)
            SelectLevel lid     -> (levelName lid, grayButton)
            StartCampaign       -> ("Campaign", grayButton)

drawPlayerButton :: PlayerSelect -> (PlayerSelect, Float) -> Picture
drawPlayerButton select (opt, mx) = translate mx playerBtnY $ pictures [
    color (if chosen then yellowButton else grayButton) (rectangleSolid playerBtnW playerBtnH),
    label (-45, -5) 0.14 (if chosen then black else white) name ]
    where
        chosen = select == opt
        name = case opt of
            SinglePlayer -> "Single Player"
            TwoPlayer    -> "Two Players"

-- ================ Playing ===================

drawPlaying :: SpriteAtlas -> GameState -> Picture
drawPlaying atlas gs = pictures [
    fullScreen black,
    drawBackground atlas gs,
    translate (negate (cameraX gs)) (negate (gsCamY gs)) $ pictures (
        map (drawStatic atlas debug) (lvlStatics lvl ++ lvlPlatforms lvl)
        ++ map (drawGoomba atlas debug) (gsGoombas gs)
        ++ map (drawBro atlas debug) (gsBros gs)
        ++ map (drawHammer atlas debug) (gsHammers gs)
        ++ map (drawBoss atlas debug (gsPlayers gs)) (maybe [] pure (gsBoss gs))
        ++ zipWith (drawPlayer atlas debug) "ML" (gsPlayers gs)
        ++ [ drawGoal (goalHitbox g) | debug, g <- lvlGoals lvl ]),   -- goals: invisible, except when debugging
    cornerLabel (levelName (lvlId lvl)),
    label (230, 270) 0.15 white ("SCORE " ++ show (gsScore gs)),
    pictures (zipWith3 (healthRow atlas) [0 ..] ["MARIO", "LUIGI"] (gsPlayers gs)) ]
    where
        lvl = gsLevel gs
        debug = gsDebug gs

cornerLabel :: String -> Picture
cornerLabel = label (-380, 270) 0.12 white

drawGoal :: Hitbox -> Picture
drawGoal hb = at (hbCenter hb) $ color green $ rectangleWire (hbWidth hb) (hbHeight hb)

healthRow :: SpriteAtlas -> Int -> String -> Player -> Picture
healthRow atlas row name p = translate (-380) (232 - 34 * fromIntegral row) $ pictures (
    label (0, -5) 0.1 white name
    : [ translate (85 + 34 * fromIntegral i) 0 (heart (i < playerHealth p)) | i <- [0 .. maxHealth - 1] ])
    where
        heart full = case lookup (if full then "FullHeart" else "EmptyHeart") atlas of
            Just pic -> scale pixelScale pixelScale pic
            Nothing  -> color (if full then red else greyN 0.4) $ circleSolid 12

-- ==================== Camera =================

cameraRange :: GameState -> (Float, Float)
cameraRange gs
    | null statics = (0, 0)
    | right < left = let mid = (left + right) / 2 in (mid, mid)   -- level narrower than the screen
    | otherwise    = (left, right)
    where
        statics = lvlStatics (gsLevel gs)
        left    = minimum (map (hbLeft . staticHitbox) statics) + halfW
        right   = maximum (map (hbRight . staticHitbox) statics) - halfW

cameraX :: GameState -> Float
cameraX gs
    | null (lvlStatics (gsLevel gs)) = mid
    | otherwise = max lo (min hi mid)
    where
        alive    = filter (not . playerDead) (gsPlayers gs)
        xs       = map (x . playerPos) (if null alive then gsPlayers gs else alive)
        mid      = if null xs then 0 else (minimum xs + maximum xs) / 2
        (lo, hi) = cameraRange gs

-- ===================== Background ============================

data Scroll = Parallax | WithLevel

data Layer = Layer {
    lySprite :: String,
    lySize   :: (Float, Float),
    lyBottom :: Float,
    lyScroll :: Scroll}

groundY :: Float
groundY = -halfH + 64

-- Fill colour for where no image reaches
backgroundFor :: LevelId -> Maybe (Color, [Layer])
backgroundFor Level1 = Just (skyBlue,
    [ Layer "Clouds" (4467, 83) (groundY + 250) Parallax
    , Layer "1-1-Background" (3584, 104) groundY WithLevel ])
backgroundFor Level3 = Just (black, [Layer "BossFightBackground" (208, 63) (-halfH - 64) Parallax])
backgroundFor _      = Nothing

drawBackground :: SpriteAtlas -> GameState -> Picture
drawBackground atlas gs = case backgroundFor (lvlId (gsLevel gs)) of
    Nothing             -> blank
    Just (fill, layers) -> pictures (fullScreen fill : map layer layers)
    where
        (lo, hi) = cameraRange gs
        progress = if hi > lo then (cameraX gs - lo) / (hi - lo) else 0.5
        layer ly = case lookup (lySprite ly) atlas of
            Nothing  -> blank
            Just pic ->
                let (pw, ph) = lySize ly
                    s    = max pixelScale (fromIntegral (ceiling (2 * halfW / pw) :: Int))
                    bw   = pw * s
                    left = case lyScroll ly of
                        Parallax  -> -halfW - progress * (bw - 2 * halfW)
                        WithLevel -> (lo - halfW) - cameraX gs
                in translate (left + bw / 2) (lyBottom ly + ph * s / 2) (scale s s pic)

-- ===================== Level and characters ===================

-- Blocks
drawStatic :: SpriteAtlas -> Bool -> StaticObj -> Picture
drawStatic atlas debug s = at (soCenter s) $
    if debug then color silverWire (rectangleWire w h)
    else maybe (color white (rectangleSolid w h)) (tiled (textureSize (soSprite s)) w h) (lookup (soSprite s) atlas)
    where
        w = soWidth s
        h = soHeight s

-- Pixel sizes
textureSize :: String -> (Float, Float)
textureSize "Platform" = (16, 8)
textureSize _          = (16, 16)

tiled :: (Float, Float) -> Float -> Float -> Picture -> Picture
tiled (texW, texH) w h pic = pictures
    [ translate (-w / 2 + tw * (fromIntegral i + 0.5)) (-h / 2 + th * (fromIntegral j + 0.5)) $
        scale (tw / texW) (th / texH) pic
    | i <- [0 .. nx - 1], j <- [0 .. ny - 1] ]
    where
        nx = max 1 (round (w / (pixelScale * texW))) :: Int
        ny = max 1 (round (h / (pixelScale * texH))) :: Int
        tw = w / fromIntegral nx
        th = h / fromIntegral ny

drawPlayer :: SpriteAtlas -> Bool -> Char -> Player -> Picture
drawPlayer atlas debug ch p = drawSprite atlas debug (playerHitbox p) sprite normal white
    where
        dir = if facingRight p then 'R' else 'L'
        sprite
            | playerDead p = [ch, 'D']
            | onGround p   = [ch, dir] ++ frame (walkPhase p) 3
            | otherwise    = [ch, 'J', dir]

drawGoomba :: SpriteAtlas -> Bool -> Goomba -> Picture
drawGoomba atlas debug gb = drawSprite atlas debug (goombaHitbox gb) sprite normal (makeColor 0.6 0.3 0.1 1.0)
    where
        sprite = case gState gb of
            Walking -> "GW" ++ frame (gPhase gb) 3
            Flat _  -> "GF"
            Dying   -> "GD"


drawBro :: SpriteAtlas -> Bool -> HammerBro -> Picture
drawBro atlas debug b = drawSprite atlas debug box sprite size (makeColor 0.2 0.7 0.3 1.0)
    where
        hb = broHitbox b
        (sprite, (dx, dy), size)
            | broDead b             = ("HI0", (0, 0), (pixelScale, -pixelScale))
            | Just _ <- broWindup b = ("HT", (8, 7), normal)
            | otherwise             = ("HI" ++ frame (broAnim b / 0.4) 2, (0, 0), normal)
        box = if debug then hb else hb { hbCenter = Vec2 (x (broPos b) + dx) (y (broPos b) + dy) }

-- Spin. The. HAMMER.
drawHammer :: SpriteAtlas -> Bool -> Hammer -> Picture
drawHammer atlas debug h = drawSprite atlas debug (hammerHitbox h) ("H" ++ frame (hamAnim h * 14) 7) normal white


drawBoss :: SpriteAtlas -> Bool -> [Player] -> Boss -> Picture
drawBoss atlas debug players b
    | bossHealth b <= 0 || flicker = blank
    | otherwise = drawSprite atlas debug (bossHitbox b) sprite (3 * facing, 3) (makeColor 0.8 0.1 0.1 1.0)
    where
        bx      = x (bossPos b)
        hurt    = bossInvisible b > 0
        flicker = hurt && even (floor (bossInvisible b * 10) :: Int)
        busy    = bossJumping b || bossCooldown b < 0.4
        sprite  = if busy && not hurt then "BT" ++ frame (bossAnim b * 8) 2 else "BTN"
        nearest = case [ x (playerPos p) | p <- players, not (playerDead p) ] of
            [] -> bx
            ps -> snd (minimum [ (abs (px - bx), px) | px <- ps ])
        facing  = if nearest > bx then -1 else 1

-- ===================== Game won / game over =====================

drawResult :: Bool -> Int -> [Highscore] -> Picture
drawResult won score scores = pictures (
    [ fullScreen black
    , centered 0.45 (if won then yellowButton else red) 190 (if won then "GAME WON" else "GAME OVER")
    , centered 0.2 white 120 ("YOUR SCORE  " ++ show score)
    , centered 0.15 grayButton 60 "PREVIOUS SCORES" ]
    ++ zipWith scoreLine [1 :: Int ..] (take 8 scores)
    ++ [ centered 0.11 grayButton (-260) "Press any key" ])
    where
        scoreLine i hs = label (-170, 20 - 30 * fromIntegral i) 0.13 white
            (show i ++ ".  " ++ padRight 14 (levelName (hsLevel hs)) ++ show (hsScore hs))
        padRight n s = s ++ replicate (n - length s) ' '
