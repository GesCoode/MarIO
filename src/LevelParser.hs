module LevelParser (parseLevelFile) where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

import GameData ( 
    BroStart (..),
    GoombaStart (..),
    LevelFile (..),
    PlayerStart (..),
    StaticObj (..),
    Vec2 (Vec2),
    Switch (..),
    SwitchType (..),
    LevelId (..))

-- | Parser returns a value and the not-yet processed input. Errors are shown using Left. 
type Parser a = String -> Either String (a, String)

-- | Read full file. Trailing input is not allowed. 
parseLevelFile :: String -> Either String LevelFile
parseLevelFile input = do
    (level, rest) <- levelFile (skipSpace input)
    case skipSpace rest of
        ""       -> validateLevel level
        leftover -> Left ("Unexpected trailing input: " ++ take 40 leftover)

-- | Check constraints for full level 
validateLevel :: LevelFile -> Either String LevelFile 
validateLevel level
    | null (lfPlayers level) = Left "Level must contains at least one player's starting position."
    | length (lfGoals level) > 1 = Left "Level may contain max. one goal."
    | length (lfBosses level) > 1 = Left "A level may contain max. one boss."
    | otherwise = Right level
    
-- | Level contains platforms, players, Goomba's, and Hammer Bros. 
--   Enemy-lists are optional, as well as goals, bosses, and switches.
levelFile :: Parser LevelFile
levelFile input = do
    ((), s1) <- expect '[' input
    (statics, s2) <- listOf staticObj (skipSpace s1)
    ((), s3) <- expect ',' (skipSpace s2)
    (players, s4) <- listOf playerStart (skipSpace s3)
    (goombas, s5) <- optionalList goombaStart s4
    (bros, s6) <- optionalList broStart s5
    (goals, s7) <- optionalList readPos s6
    (bosses, s8) <- optionalList readPos s7
    (switches, s9) <- optionalList readSwitch s8
    ((), s10) <- expect ']' (skipSpace s9)
    Right (LevelFile statics players goombas bros goals bosses switches, s10)

-- | Read an optional list, which is preceded by a comma.
--   Otherwise an empty list.
optionalList :: Parser a -> Parser [a]
optionalList item s0 = 
    case skipSpace s0 of
        ',' : rest -> listOf item (skipSpace rest)
        rest       -> Right ([], rest)

-- | Read position: [x, y] 
readPos :: Parser Vec2
readPos input = do
    ((), s1) <- expect '[' input
    (cx, s2) <- number (skipSpace s1)
    ((), s3) <- expect ',' (skipSpace s2)
    (cy, s4) <- number (skipSpace s3)
    ((), s5) <- expect ']' (skipSpace s4)
    Right (Vec2 cx cy, s5)
    
-- | Read Hammer Bro-startingposition: [x, y].
broStart :: Parser BroStart
broStart input = do
    (position, rest) <- readPos input
    Right (BroStart position, rest)

-- | Read Goomba: [x, y, dir]
--   Direction: -1 (left) or 1 (right).
goombaStart :: Parser GoombaStart
goombaStart s0 = do
    ((), s1) <- expect '[' s0
    (cx, s2) <- number (skipSpace s1)
    ((), s3) <- expect ',' (skipSpace s2)
    (cy, s4) <- number (skipSpace s3)
    ((), s5) <- expect ',' (skipSpace s4)
    (dir, s6) <- number (skipSpace s5)
    ((), s7) <- expect ']' (skipSpace s6)
    if dir == -1 || dir == 1
        then Right (GoombaStart (Vec2 cx cy) dir, s7)
        else Left "Invalid Goomba direction: use -1 or 1."

-- | Read a platform: [x, y, w, h, "sprite"]        
staticObj :: Parser StaticObj
staticObj input = do
    ((), s1) <- expect '[' input
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
    if w > 0 && h > 0 && not (null sprite)
        then Right (StaticObj (Vec2 cx cy) w h sprite, s11)
        else Left "A platform must have positive dimensions and a spritename"
         
-- | Read a player initialisation: [x, y, vx, vy]   
playerStart :: Parser PlayerStart
playerStart input = do
    ((), s1) <- expect '[' input
    (cx, s2) <- number (skipSpace s1)
    ((), s3) <- expect ',' (skipSpace s2)
    (cy, s4) <- number (skipSpace s3)
    ((), s5) <- expect ',' (skipSpace s4)
    (vx, s6) <- number (skipSpace s5)
    ((), s7) <- expect ',' (skipSpace s6)
    (vy, s8) <- number (skipSpace s7)
    ((), s9) <- expect ']' (skipSpace s8)
    Right (PlayerStart (Vec2 cx cy) (Vec2 vx vy), s9)

-- | Parse a list between square brackets.
-- Ending with a comma is not allowed.
listOf :: Parser a -> Parser [a]
listOf item s0 = do
    ((), s1) <- expect '[' (skipSpace s0)
    case skipSpace s1 of 
        ']' : rest -> Right ([], rest)
        rest       -> readElems [] rest
    where
        readElems acc rest = do
            (itemVal, s2) <- item (skipSpace rest)
            case skipSpace s2 of
                ',' : s3 -> 
                    case skipSpace s3 of
                        ']' : _ -> Left "List may not end with a comma." 
                        s4 -> readElems (itemVal : acc) s4 
                ']' : s3 -> Right (reverse (itemVal : acc), s3)
                _ -> Left "Expected ',' or ']' in a list."
                
-- | Read a whole or decimal number
number :: Parser Float
number s0 = do
    let (n, rest) = case s0 of 
            '-' : s -> ("-", s)
            _       -> ("", s0)
        (whole, restWhole) = span isDigit rest
    if null whole
        then Left "Expected a number."
        else if leadingZero whole
            then Left "Leading zero's not allowed."
            else case restWhole of 
                '.' : fracInput -> 
                    case span isDigit fracInput of 
                        ("", _) -> Left "Expected digits after decimal point."
                        (frac, restN) ->
                            readFloat (n ++ whole ++ "." ++ frac) restN
                _ -> readFloat (n ++ whole) restWhole
    where 
        leadingZero ('0' : _ : _) = True
        leadingZero _             = False
        
        readFloat text rest = 
            case readMaybe text of 
                Just value 
                    | not (isNaN value || isInfinite value) -> Right(value, rest)
                _ -> Left "Invalid or too large digit"

-- | Read string between double quotes: "..."
--   No escape sequences
stringLit :: Parser String
stringLit ('"' : rest) = readText [] rest
    where
        readText _ [] = Left "Text has not been ended with a quote."
        readText acc ('"' : s) = Right (reverse acc, s)
        readText _ ('\\' : _) = Left "Escape is not allowed"
        readText acc (c : s)   
            | c < ' ' = Left "Invalid sign in text."
            | otherwise = readText (c : acc) s
stringLit _ = Left "Expected string between double quotes."

-- | Check if next char corresponds with expected char
expect :: Char -> Parser ()
expect c (ch : xs)
    | ch == c   = Right ((), xs)
    | otherwise = Left ("Expected '" ++ [c] ++ "', but got '" ++ [ch] ++ "'.")
expect c []   = Left ("Expected '" ++ [c] ++ "', but got end of input.")

skipSpace :: String -> String
skipSpace = dropWhile isSpace

-- | Switch: [x, y, "vertical"] or [x, y, "horizontal"]
--   "end" is meant as the end of a random level
readSwitch :: Parser Switch
readSwitch input = do
    ((), s1) <- expect '[' input
    (cx, s2) <- number (skipSpace s1)
    ((), s3) <- expect ',' (skipSpace s2)
    (cy, s4) <- number (skipSpace s3)
    ((), s5) <- expect ',' (skipSpace s4)
    (goal, s6) <- stringLit (skipSpace s5)
    ((), s7) <- expect ',' (skipSpace s6)
    let createSwitch spot = Right (Switch (Vec2 cx cy) spot, s7)
    case goal of 
        "vertical" -> createSwitch ToVertical
        "level1" -> createSwitch (ToHorizontal Level1)
        "level2" -> createSwitch (ToHorizontal Level2)
        "level3" -> createSwitch (ToHorizontal Level3)
        "end" -> createSwitch ToEnd
        _ -> Left ("Unknown switch type: " ++ goal ++  ".")