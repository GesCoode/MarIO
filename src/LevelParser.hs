module LevelParser (parseLevelFile) where

import GameData ( 
    BroStart (..),
    GoombaStart (..),
    LevelFile (..),
    PlayerStart (..),
    StaticObj (..),
    Vec2 (..))
import Text.ParserCombinators.ReadP ( 
    ReadP,
    between,
    char,
    eof,
    option,
    pfail,
    readP_to_S,
    readS_to_P,
    sepBy,
    skipSpaces)
    
-- | Read level (obj, players, enemies) 
parseLevelFile :: String -> Either String LevelFile 
parseLevelFile content = 
    case [lvl | (lvl, "") <- readP_to_S parse content] of
        [] -> Left "Invalid file: check please."
        (lvl : _)
            | all validObj (lfStatics lvl) -> Right lvl
            | otherwise -> Left "Object has invalid dimensions or name."
    where 
            parse = skipSpaces *> readLevel <* skipSpaces <* eof

-- | Read a value between [ ] 
lst :: ReadP a -> ReadP [a]
lst el = between (sym '[') (sym ']')
    (sepBy el (sym ','))

-- | Read a symbol + skip spaces
sym :: Char -> ReadP Char
sym tkn = skipSpaces *> char tkn <* skipSpaces

-- | Numbers 
num :: ReadP Float
num = do 
    val <- skipSpaces *> readS_to_P reads <* skipSpaces
    if isNaN val || isInfinite val then pfail else pure val 

text :: ReadP String
text = skipSpaces *> readS_to_P reads <* skipSpaces

vec :: ReadP Vec2
vec = Vec2 <$> num <* sym ',' <*> num

readLevel :: ReadP LevelFile
readLevel = between (sym '[') (sym ']') $
    LevelFile <$> lst readObj
    <* sym ',' <*> lst readPlayer
    <*> option [] (sym ',' *> lst readGoomba)
    <*> option [] (sym ',' *> lst readBro)
    
readObj :: ReadP StaticObj
readObj = between (sym '[') (sym ']') $
    StaticObj <$> vec <* sym ','
    <*> num <* sym ','
    <*> num <* sym ','
    <*> text

readPlayer :: ReadP PlayerStart
readPlayer = between (sym '[') (sym ']') $
    PlayerStart <$> vec <* sym ',' <*> vec

readGoomba :: ReadP GoombaStart
readGoomba = between (sym '[') (sym ']') $
    GoombaStart <$> vec <* sym ',' <*> num

readBro :: ReadP BroStart
readBro = between (sym '[') (sym ']') $
    BroStart <$> vec

validObj :: StaticObj -> Bool
validObj obj = 
    soWidth obj > 0 &&
    soHeight obj > 0 &&
    not (null (soSprite obj))