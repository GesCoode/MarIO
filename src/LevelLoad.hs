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

-- | Load a level JSON file for the given LevelId.
loadLevel :: LevelId -> IO Level
loadLevel lid = do
    let path = levelPath lid
    raw <- readFile path
    if not raw 
        then ioError (userError ("Levelfile does not exist: " ++ path))
        else do 
            content <- readFile path
            case parseLevelFile (stripBom raw) of
                Left err -> ioError (userError ("Failed to load (" ++ path ++ "): " ++ err))
                Right (LevelFile sts pls gbs bros goals boss) -> do
                    let level = Level { 
                        lvlId      = lid,
                        lvlStatics = sts,
                        lvlPlayers = pls,
                        lvlGoombas = gbs,
                        lvlBros    = bros,
                        lvlGoals   = goals,
                        lvlBosses  = boss}
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