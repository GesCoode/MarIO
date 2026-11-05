module LevelLoad (loadLevel) where

import GameData( 
    Hitbox (..),
    Level (..),
    LevelFile (..),
    LevelId,
    PlayerStart (..),
    StaticObj (..),
    levelPath,
    overlaps,
    staticHitbox)
    
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
    lvlSwitches = lfSwitches}

-- | Read Level; parsing continues. 
loadLevel :: LevelId -> IO Level
loadLevel lid = case levelPath lid of 
    Nothing -> ioError (userError "This level is generated and does not have a file.")
    Just path -> loadExampleLevel lid path
    
-- | Load a level JSON file for the given LevelId.
loadExampleLevel :: LevelId ->FilePath -> IO Level
loadLevel lid path = do
    exist <- doesFileExist path
    if not exist 
        then ioError (userError ("Level file is missing: " ++ path))
        else pure ()
    raw <- readFile path
    case parseLevelFile (stripBom raw) of
        Left err -> ioError (userError ("Failed to load (" ++ path ++ "): " ++ err))
        Right (LevelFile sts pls gbs bros goals boss) -> do
            let level = createLevel lid ids
            case validateStartpos level of 
                Left err -> ioError (userError ("Invalid level (" ++ path ++ "):" ++ err))
                Right () -> do 
                    -- Check all images
                    let pictures = [
                        "Asserts/" ++ soSprite obj ++ ".bmp" | 
                        obj <- lvlStatics level]
                    exist <- mapM doesFileExist pictures
                    case [name | (name, found) <- zip pictures exist, not found] of 
                        [] -> pure level
                        missing : _ -> ioError (userError ("Image missing: " ++ missing))

-- | UTF-8 BOM that Windows editors often prepend.
stripBom :: String -> String
stripBom ('\xfeff' : xs) = xs
stripBom xs              = xs

-- | Check that player doesn't start in static
validateStartpos :: Level -> Either String ()
validateStartpos level
    | any overlapStatic (lvlPlayer level) = Left "Player starts inside static."
    | otherwise = Right () 
    where 
        staticCell = map staticHitbox (lvlStatics level)
        overlapStatic start = any (overlaps playerCell) staticCell
            where 
                playerCell = Hitbox (psCenter start) 32 42