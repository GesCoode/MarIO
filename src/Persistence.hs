module Persistence (
    loadScores,
    saveScores, 
    viewScores,
    writeScores) where
    
import Control.Exception (IOException, evaluate, try)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.ParserCombinators.ReadP (
    ReadP, between, char, eof, readP_to_S, 
    readS_to_P, sepBy, skipSpaces)
import GameData (Highscore (..), LevelId (..))

scorePath :: FilePath
scorePath = "Saves/scores.json"

-- Change scores into JSON format
writeScores :: [Highscore] -> String
writeScores scores = "[" ++ intercalate "," (map writeScore scores) ++ "]\n"
    where 
        writeScore score = "[\"" ++ show (hsLevel score) ++ "\","  ++ show (hsScore score) ++ "]"

viewScores :: String -> Either String [Highscore]
viewScores input = 
    case [scores | (scores, "") <- readP_to_S parser input] of 
        [] -> Left "File is invalid."
        (scores : _) 
            | any ((<0) . hsScore) scores -> Left "Highscore can't be negative."
            | otherwise -> Right scores
        where 
            parser = skipSpaces *> list readScore <* skipSpaces <* eof  

-- | Read highscore 
readScore :: ReadP Highscore
readScore = between (token '[') (token ']') $ do
    name <- skipSpaces *> readS_to_P reads <* skipSpaces
    _ <- token ','
    pts <- skipSpaces *> readS_to_P reads <* skipSpaces
    case lookup name names of 
        Just lvl -> pure (Highscore lvl pts)
        Nothing -> fail "Unknown level"
    where 
        names = 
            [(show lvl, lvl) | lvl <- [minBound .. maxBound]] 
            ++ [ ("Sandbox", Sandbox), ("Level1", Level1), ("Level2", Level2), ("Level3", Level3)]

list :: ReadP a -> ReadP [a]
list parser = between (token '[') (token ']') (sepBy parser (token ','))

token :: Char -> ReadP Char
token c = skipSpaces *> char c <* skipSpaces

-- Load a save file
loadPersFile :: FilePath -> a -> (String -> Either String a) -> IO (Either String a)
loadPersFile path standard parser = do 
    exist <- doesFileExist path
    if not exist 
        then pure (Right standard)
        else do 
            content <- safeRead path
            pure (content >>= parser)

-- Read file and catch errs
safeRead :: FilePath -> IO (Either String String)
safeRead path = do 
    res <- try $ do 
        content <- readFile path
        _ <- evaluate (length content)
        pure content
    pure $ case (res :: Either IOException String) of
        Left err -> Left ("Reading file failed: " ++ show err)
        Right content -> Right content 

-- Write text to file 
savePersFile :: FilePath -> String -> IO ()
savePersFile path input = do 
    createDirectoryIfMissing True "Saves"
    writeFile path input 
    
loadScores :: IO (Either String [Highscore])
loadScores = loadPersFile scorePath [] viewScores

saveScores :: [Highscore] -> IO ()
saveScores scores = savePersFile scorePath (writeScores scores)
