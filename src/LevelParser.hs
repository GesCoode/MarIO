module LevelParser (parseLevelFile) where

import Data.Char (isDigit, isSpace)

import GameData
  ( BroStart (..)
  , GoombaStart (..)
  , Hitbox (..)
  , LevelFile (..)
  , PlayerStart (..)
  , StaticObj (..)
  , Vec2 (Vec2)
  )

type Parser a = String -> Either String (a, String)

parseLevelFile :: String -> Either String LevelFile
parseLevelFile input = do
  (file, rest) <- levelFile (skipSpace input)
  case skipSpace rest of
    ""       -> Right file
    leftover -> Left ("Unexpected trailing input: " ++ take 40 leftover)

levelFile :: Parser LevelFile
levelFile s0 = do
  ((), s1) <- expect '[' s0
  (statics, s2) <- listOf staticObj (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (players, s4) <- listOf playerStart (skipSpace s3)
  -- optional third list: goombas
  (goombas, s5) <- optionalList goombaStart s4
  -- optional fourth list: hammer bros
  (bros, s6) <- optionalList broStart s5
  -- optional fifth list: finish boxes
  (finish, s7) <- optionalList finishBox s6
  ((), s8) <- expect ']' (skipSpace s7)
  Right (LevelFile statics players goombas bros finish, s8)

-- | @, [ ... ]@ if present, otherwise an empty list.
optionalList :: Parser a -> Parser [a]
optionalList item s0 = case skipSpace s0 of
  ',' : s -> listOf item (skipSpace s)
  s       -> Right ([], s)

-- | Finish box: @[cx, cy, w, h]@ (center, width, height).
finishBox :: Parser Hitbox
finishBox s0 = do
  ((), s1) <- expect '[' s0
  (cx, s2) <- number (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (cy, s4) <- number (skipSpace s3)
  ((), s5) <- expect ',' (skipSpace s4)
  (w, s6) <- number (skipSpace s5)
  ((), s7) <- expect ',' (skipSpace s6)
  (h, s8) <- number (skipSpace s7)
  ((), s9) <- expect ']' (skipSpace s8)
  Right (Hitbox (Vec2 cx cy) w h, s9)

-- | Hammer Bro: @[cx, cy]@.
broStart :: Parser BroStart
broStart s0 = do
  ((), s1) <- expect '[' s0
  (cx, s2) <- number (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (cy, s4) <- number (skipSpace s3)
  ((), s5) <- expect ']' (skipSpace s4)
  Right (BroStart (Vec2 cx cy), s5)

-- | Goomba: @[cx, cy, dir]@ with dir -1 (left) or 1 (right).
goombaStart :: Parser GoombaStart
goombaStart s0 = do
  ((), s1) <- expect '[' s0
  (cx, s2) <- number (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (cy, s4) <- number (skipSpace s3)
  ((), s5) <- expect ',' (skipSpace s4)
  (d, s6) <- number (skipSpace s5)
  ((), s7) <- expect ']' (skipSpace s6)
  Right (GoombaStart (Vec2 cx cy) d, s7)

staticObj :: Parser StaticObj
staticObj s0 = do
  ((), s1) <- expect '[' s0
  (cx, s2) <- number (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (cy, s4) <- number (skipSpace s3)
  ((), s5) <- expect ',' (skipSpace s4)
  (w, s6) <- number (skipSpace s5)
  ((), s7) <- expect ',' (skipSpace s6)
  (h, s8) <- number (skipSpace s7)
  ((), s9) <- expect ',' (skipSpace s8)
  (sprite, s10) <- stringLit (skipSpace s9)
  ((), s11) <- expect ']' (skipSpace s10)
  Right (StaticObj (Vec2 cx cy) w h sprite, s11)

playerStart :: Parser PlayerStart
playerStart s0 = do
  ((), s1) <- expect '[' s0
  (cx, s2) <- number (skipSpace s1)
  ((), s3) <- expect ',' (skipSpace s2)
  (cy, s4) <- number (skipSpace s3)
  ((), s5) <- expect ',' (skipSpace s4)
  (vx, s6) <- number (skipSpace s5)
  ((), s7) <- expect ',' (skipSpace s6)
  (vy, s8) <- number (skipSpace s7)
  ((), s9) <- expect ']' (skipSpace s8)
  Right (PlayerStart (Vec2 cx cy) (Vec2 vx vy), s9)

-- | Parse @[ item, item, ... ]@.
listOf :: Parser a -> Parser [a]
listOf item s0 = do
  ((), s1) <- expect '[' s0
  go [] (skipSpace s1)
  where
    go acc s =
      case skipSpace s of
        ']' : rest -> Right (reverse acc, rest)
        s' -> do
          (itemVal, s2) <- item s'
          case skipSpace s2 of
            ',' : s3 -> go (itemVal : acc) (skipSpace s3)
            ']' : s3 -> Right (reverse (itemVal : acc), s3)
            _        -> Left "Expected ',' or ']' in array"

number :: Parser Float
number s0 =
  case s0 of
    '-' : s1 -> do
      (n, rest) <- unsigned s1
      Right (-n, rest)
    _ -> unsigned s0
  where
    unsigned s =
      case span isDigit s of
        ("", _) -> Left "Expected number"
        (intPart, rest) ->
          case rest of
            '.' : frac ->
              case span isDigit frac of
                ("", _) -> Left "Expected digits after decimal point"
                (fracPart, rest') ->
                  Right (read (intPart ++ '.' : fracPart), rest')
            _ -> Right (read intPart, rest)

-- | Simple string: @"..."@ with no escape sequences.
stringLit :: Parser String
stringLit ('"' : s) = go "" s
  where
    go _ ""           = Left "Unterminated string"
    go acc ('"' : s') = Right (reverse acc, s')
    go acc (c : s')   = go (c : acc) s'
stringLit _ = Left "Expected string"

expect :: Char -> Parser ()
expect c (ch : xs)
  | ch == c   = Right ((), xs)
  | otherwise = Left ("Expected '" ++ [c] ++ "', got '" ++ [ch] ++ "'")
expect c []   = Left ("Expected '" ++ [c] ++ "', got end of input")

skipSpace :: String -> String
skipSpace = dropWhile isSpace
