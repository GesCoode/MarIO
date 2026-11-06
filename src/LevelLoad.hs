module LevelLoad (loadLevel) where

import GameData( 
    Hitbox (..),
    Level (..),
    LevelFile (..),
    LevelId,
    PlayerStart (..),
    StaticObj (..),
    levelPath,
    HasHitbox (..),
    overlaps)
    
import LevelParser (parseLevelFile)
import System.Directory (doesFileExist)

createLevel :: LevelId -> LevelFile -> Level
createLevel lid ids = Level {
    lvlId = lid,
    lvlStatics = lfStatics ids, 
    lvlPlayers = lfPlayers ids,
    lvlGoombas = lfGoombas ids,
    lvlBros = lfBros ids,
    lvlGoals = lfGoals ids,
    lvlBosses = lfBosses ids,
    lvlSwitches = lfSwitches ids,
    lvlPlatforms = []}

-- Read Level
loadLevel :: LevelId -> IO Level
loadLevel lid = case levelPath lid of 
    Nothing -> ioError (userError "This level is generated and does not have a file.")
    Just path -> loadExampleLevel lid path
    
-- Load a level JSON file for the given LevelId.
loadExampleLevel :: LevelId ->FilePath -> IO Level
loadExampleLevel lid path = do
    exist <- doesFileExist path
    if not exist 
        then ioError (userError ("Level file is missing: " ++ path))
        else pure ()
    raw <- readFile path
    case parseLevelFile (stripBom raw) of
        Left err -> ioError (userError ("Failed to load (" ++ path ++ "): " ++ err))
        Right ids -> do
            let level = createLevel lid ids
            case validateStartpos level of 
                Left err -> ioError (userError ("Invalid level (" ++ path ++ "):" ++ err))
                Right () -> do 
                    -- Check all images
                    let pictures = [ "Assets/" ++ soSprite obj ++ ".bmp" 
                                   | obj <- lvlStatics level]
                    existing <- mapM doesFileExist pictures
                    case [name | (name, found) <- zip pictures existing, not found] of 
                        [] -> pure level
                        missing : _ -> ioError (userError ("Image missing: " ++ missing))

stripBom :: String -> String
stripBom ('\xfeff' : xs) = xs
stripBom xs              = xs

-- Extra double checking the player
validateStartpos :: Level -> Either String ()
validateStartpos level
    | any overlapStatic (lvlPlayers level) = Left "Player starts inside static."
    | otherwise = Right () 
    where 
        staticCell = map hitbox (lvlStatics level)
        overlapStatic start = any (overlaps playerCell) staticCell
            where 
                playerCell = Hitbox (psCenter start) 32 42