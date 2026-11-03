module Controller
  ( handleEvent
  , stepApp
  ) where

import Graphics.Gloss.Interface.IO.Game
  ( Event (..)
  , Key (..)
  , KeyState (..)
  , MouseButton (..)
  , SpecialKey (..)
  )

import GameData
  ( AppState (..)
  , GameState (..)
  , InputHandler (..)
  , Level (..)
  , Player (..)
  , LevelId (..)
  , Screen (..)
  , initialApp
  , mkGameState
  )
import LevelLoad (loadLevel)
import Menu (hitTest)
import Physics (stepPlayer)

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) app =
  case screen app of
    Menu -> case hitTest mouse of
      Nothing  -> pure app
      Just lid -> do
        lvl <- loadLevel lid
        pure app { screen = Playing (mkGameState lvl) }
    Playing _ -> pure app

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) app =
  case screen app of
    Playing _ -> pure (initialApp (sprites app))
    Menu      -> pure app

-- 'c' toggles wireframe view (no textures).
handleEvent (EventKey (Char 'c') Down _ _) app =
  pure $ mapPlaying app $ \gs -> gs { gsDebug = not (gsDebug gs) }

-- 'r' resets the sandbox (keeps debug setting).
handleEvent (EventKey (Char 'r') Down _ _) app =
  pure $ mapPlaying app resetSandbox

handleEvent (EventKey key keyState _ _) app =
  case keyState of
    Down -> pure $ mapPlaying app (applyKey key True)
    Up   -> pure $ mapPlaying app (applyKey key False)

handleEvent _ app = pure app

mapPlaying :: AppState -> (GameState -> GameState) -> AppState
mapPlaying app f = case screen app of
  Playing gs -> app { screen = Playing (f gs) }
  Menu       -> app
  
resetSandbox :: GameState -> GameState
resetSandbox gs
  | lvlId (gsLevel gs) == Sandbox = (mkGameState (gsLevel gs)) { gsDebug = gsDebug gs }
  | otherwise                     = gs

-- Player 0: WASD. Player 1: arrow keys.
applyKey :: Key -> Bool -> GameState -> GameState
applyKey key pressed gs = case key of
  Char 'a'            -> onPlayer 0 (\i -> i { inLeft  = pressed })
  Char 'd'            -> onPlayer 0 (\i -> i { inRight = pressed })
  Char 'w'            -> onPlayer 0 (\i -> i { inJump  = pressed })
  SpecialKey KeyLeft  -> onPlayer 1 (\i -> i { inLeft  = pressed })
  SpecialKey KeyRight -> onPlayer 1 (\i -> i { inRight = pressed })
  SpecialKey KeyUp    -> onPlayer 1 (\i -> i { inJump  = pressed })
  _                   -> gs
  where
    onPlayer n f = gs { gsPlayers = zipWith (upd n f) [0 ..] (gsPlayers gs) }
    upd n f i pl
      | i == (n :: Int) = pl { playerIn = f (playerIn pl) }
      | otherwise       = pl

stepApp :: Float -> AppState -> IO AppState
stepApp dt app = pure $ mapPlaying app (stepPlayer dt)
