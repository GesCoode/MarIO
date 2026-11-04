module LevelLoad (loadLevel) where

import GameData
  ( Level (..)
  , LevelFile (..)
  , LevelId
  , levelPath
  )
import LevelParser (parseLevelFile)

-- | Load a level JSON file for the given LevelId.
loadLevel :: LevelId -> IO Level
loadLevel lid = do
  let path = levelPath lid
  raw <- readFile path
  case parseLevelFile (stripBom raw) of
    Left err ->
      ioError (userError ("Failed to load " ++ path ++ ": " ++ err))
    Right (LevelFile sts pls gbs bros) ->
      pure Level
        { lvlId      = lid
        , lvlStatics = sts
        , lvlPlayers = pls
        , lvlGoombas = gbs
        , lvlBros    = bros
        }

-- | UTF-8 BOM that Windows editors often prepend.
stripBom :: String -> String
stripBom ('\xfeff' : xs) = xs
stripBom xs              = xs
