module Controller ( 
    handleEvent,
    stepApp,
    activateSwitch) 
    where

import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.Gloss.Interface.IO.Game (
    Event (..),
    Key (..),
    KeyState (..),
    MouseButton (..),
    SpecialKey (..))

import GameData (
    AppState (..),
    GameMode(..),
    GameState (..),
    GameStatus (..),
    Highscore (..),
    InputHandler (..),
    Level (..),
    LevelId (..),
    Player (..),
    Screen (..),
    SwitchType (..),
    mkGameStateMode)
    
import LevelLoad (loadLevel)
import Menu (MenuSelect (..), hitTest, hitTestPlayers)
-- import Persistence (saveScores, saveProgress)
import Physics (stepWorld, findSwitch)
import Procedural (createVert, nextSeed, checkAvail)

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) app =
    case screen app of
        Menu -> case hitTestPlayers mouse of
            Just select -> pure app { playerSelect = select }
            Nothing -> case hitTest mouse of 
                Nothing  -> pure app
                Just lvl -> startLevel lvl app
        Playing _ -> pure app
        
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) app =
    pure app { screen = Menu }

-- | Pause, toggel or restart
handleEvent (EventKey (Char tkn) Down _ _) app
    | toLower tkn == 'p' = 
        pure (mapPlaying app (\gs -> gs { gsPaused = not (gsPaused gs) }))
    | toLower tkn == 'c' = 
        pure (mapPlaying app (\gs -> gs { gsDebug = not (gsDebug gs) }))
    | toLower tkn == 'r' = 
        pure (mapPlaying app resetSandbox)
        
-- | Process pressing and letting go of movement keys 
handleEvent (EventKey key keyState _ _) app =
    pure (mapPlaying app (applyKey key (keyState == Down)))
handleEvent _ app = pure app

-- | Load level and create chosen players
--   Loading error: menu stays visible --> error goes to stderr
startLevel :: LevelId -> AppState -> IO AppState
startLevel lvl app = do
    res <- try (loadLevel lvl) :: IO (Either IOException Level)
    case res of 
        Left err -> do
            PutStrLn stderr ("Level couldn't be loaded: " ++ show err)
            pure app
        Right loaded -> 
            case mkGameState (playerSelect app) loaded of 
                Left msg -> do
                    hPutStrLn stderr msg
                    pure app
                Right gs -> pure app { screen = Playing gs }

-- | Adjust pure function only if game is active     
mapPlaying :: AppState -> (GameState -> GameState) -> AppState
mapPlaying app f = case screen app of
    Playing gs -> app { screen = Playing (f gs) }
    Menu       -> app

resetSandbox :: GameState -> GameState
resetSandbox gs
  | lvlId (gsLevel gs) /= Sandbox = gs
  | otherwise = case mkGameState (gsPlayerSelect gs) (gsLevel gs) of
      Left _ -> gs
      Right new -> new { gsDebug = gsDebug gs }

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
stepApp dt app = pure $ mapPlaying app (stepWorld dt)
