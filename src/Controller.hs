module Controller (handleEvent, stepApp, activateSwitch) where

import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.Gloss.Interface.IO.Game 
    (Event (..), Key (..), KeyState (..), MouseButton (..), SpecialKey (..))

import GameData 
    (AppState (..), GameMode(..), GameState (..), GameStatus (..), Highscore (..), 
    InputHandler (..), Level (..), LevelId (..), Player (..), Screen (..), 
    SwitchType (..), mkGameStateMode)
    
import LevelLoad (loadLevel)
import Menu (MenuSelect (..), hitTest, hitTestPlayers)
import Persistence (saveScores, saveProgress)
import Physics (stepWorld, findSwitch)
import Procedural (createVert, nextSeed, checkAvail)

handleEvent :: Event -> AppState -> IO AppState
-- | Result screen: any key or click goes back to the menu
handleEvent (EventKey _ Down _ _) app
    | Result _ _ <- screen app = pure app { screen = Menu, message = Nothing }
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) app =
    case screen app of
        Menu -> case hitTestPlayers mouse of
            Just select -> pure app { playerSelect = select }
            Nothing -> case hitTest mouse of 
                Nothing  -> pure app
                Just (SelectLevel level) -> startLevel level LevelSelect app
                Just StartCampaign -> startLevel Level1 Campaign app
        _ -> pure app

-- | First player activates with E and second with Enter
handleEvent (EventKey (Char c) Down _ _) app
    | toLower c == 'e' = activateSwitch 0 app
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) app = 
    activateSwitch 1 app

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) app =
    pure app { screen = Menu, message = Nothing }

-- | Pause, toggle, restart, nexy
handleEvent (EventKey (Char c) Down _ _) app
    | toLower c == 'p' = 
        pure (mapPlaying app (\gs -> gs { gsPaused = not (gsPaused gs) }))
    | toLower c == 'c' = 
        pure (mapPlaying app (\gs -> gs { gsDebug = not (gsDebug gs) }))
    | toLower c == 'r' = pure (mapPlaying app restart)
    | toLower c == 'n' = nextStep app
        
-- | Process pressing and letting go of movement keys 
handleEvent (EventKey key keyState _ _) app =
    pure (mapPlaying app (applyKey key (keyState == Down)))
handleEvent _ app = pure app

-- | Generate and start a random level. In the campaign its exit leads to the boss.
startRandom :: GameMode -> AppState -> AppState
startRandom mode app
    | not (checkAvail level) = app { message = Just "The generated level is not available." }
    | otherwise = case mkGameStateMode (playerSelect app) mode level of 
        Left msg -> app { message = Just msg }
        Right gs -> app {
            screen = Playing gs,
            message = Nothing,
            seed = nextSeed (seed app) }
    where 
        level = createVert (seed app) exit
        exit = case mode of
            Campaign    -> ToHorizontal Level3
            LevelSelect -> ToEnd

-- | Switch between horizontal and vertical 
activateSwitch :: Int -> AppState -> IO AppState
activateSwitch n app = case screen app of 
    Playing gs 
        | gsPaused gs || gsStatus gs /= InProgress -> pure app
        | otherwise -> case findSwitch n gs of 
            Nothing -> pure app
            Just ToVertical -> pure (startVert app gs)
            Just (ToHorizontal lid) -> contCampaign lid app gs
            Just ToEnd -> saveWin
                (app { screen = Playing (gs { gsStatus = Win }) })
                (gs { gsStatus = Win })
    _ -> pure app 
    
startVert :: AppState -> GameState -> AppState
startVert app gs 
    | not (checkAvail level) = app { message = Just "The generated level is not available." }
    | otherwise = case mkGameStateMode (gsPlayerSelect gs) (gsMode gs) level of 
        Left msg -> app { message = Just msg }
        Right new -> app {
            screen = Playing (keepHealth gs (new { 
            gsScore = gsScore gs, 
            gsStartScore = gsScore gs, 
            gsDebug = gsDebug gs})),
        message = Nothing,
        seed = nextSeed (seed app) }
    where 
        level = createVert (seed app) exit
        exit = case gsMode gs of 
            Campaign -> case nextLevel (lvlId (gsLevel gs)) of
                Just lid -> ToHorizontal lid
                Nothing -> ToEnd
            LevelSelect -> ToEnd
            
-- | Continue campaign: read next level and take scored points with you
contCampaign :: LevelId -> AppState -> GameState -> IO AppState
contCampaign lid app prev = do
    res <- startLevel lid (gsMode prev)
        (app { playerSelect = gsPlayerSelect prev })
    case (message res, screen res) of 
        (Just _, _) -> pure res
        (Nothing, Playing new) -> do
            let progress = max (levelUnlocked app) (nLevel lid)
                cont = res {
                    screen = Playing (keepHealth prev (new { 
                        gsScore = gsScore prev,
                        gsStartScore = gsScore prev, 
                        gsDebug = gsDebug prev})),
                    levelUnlocked = progress }
            if gsMode prev /= Campaign 
                then pure (cont { levelUnlocked = levelUnlocked app })
                else do 
                    result <- try (saveProgress progress) :: IO (Either IOException ())
                    pure $ case result of 
                        Left err -> cont { message = Just ("Saving progress failed: " ++ show err) }
                        Right () -> cont
        _ -> pure res 
            
startLevel :: LevelId -> GameMode -> AppState -> IO AppState
startLevel RandomVertical mode app = pure (startRandom mode app)   -- generated, no file
startLevel lid mode  app = do
    res <- try (loadLevel lid) :: IO (Either IOException Level)
    pure $ case res of 
        Left err -> app { message = Just ("Loading level failed: " ++ show err) }
        Right level -> case mkGameStateMode (playerSelect app) mode level of 
            Left msg -> app { message = Just msg }
            Right gs -> app {
                screen = Playing gs,
                message = Nothing }

keepHealth :: GameState -> GameState -> GameState
keepHealth prev new = new {
    gsPlayers = zipWith keep (gsPlayers prev) (gsPlayers new) }
    where 
        keep oldPl newPl = newPl {
            playerHealth = playerHealth oldPl,
            playerDead = playerHealth oldPl <= 0}
            
restart :: GameState -> GameState
restart gs = case mkGameStateMode (gsPlayerSelect gs) (gsMode gs) (gsLevel gs) of
    Left _ -> gs
    Right new -> new {
        gsDebug = gsDebug gs,
        gsScore = gsStartScore gs,
        gsStartScore = gsStartScore gs }
        
-- | Adjust pure function only if game is active     
mapPlaying :: AppState -> (GameState -> GameState) -> AppState
mapPlaying app f = case screen app of
    Playing gs -> app { screen = Playing (f gs) }
    _          -> app

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

-- | N closes completed campaign or start a new level
nextStep :: AppState -> IO AppState
nextStep app = case screen app of 
    Playing gs 
        | gsStatus gs == Win && gsMode gs == Campaign -> 
            case nextLevel (lvlId (gsLevel gs )) of 
                Just lid -> do 
                    new <- startLevel lid Campaign app
                    pure $ case screen new of 
                        Playing newGame -> new {
                            screen = Playing (keepHealth gs (newGame { 
                                gsScore = gsScore gs, 
                                gsStartScore = gsScore gs })) }
                        _ -> new 
                -- last level of the campaign: game won
                Nothing -> pure app { screen = Result True (gsScore gs) }
    _ -> pure app 
    
nLevel :: LevelId -> Int
nLevel Level1 = 1 
nLevel Level2 = 2 
nLevel RandomVertical = 2
nLevel Level3 = 3 
nLevel _ = 1 

nextLevel :: LevelId -> Maybe LevelId
nextLevel Level1 = Just RandomVertical
nextLevel RandomVertical = Just Level3
nextLevel Level2 = Just Level3
nextLevel _ = Nothing 
 
stepApp :: Float -> AppState -> IO AppState
stepApp dt app = case screen app of 
    Playing old -> do 
        let new = stepWorld dt old 
            cont = app { screen = Playing new }
        case (gsStatus old, gsStatus new) of
            (InProgress, Win) -> do
                saved <- saveWin cont new
                -- campaign: straight on to the next level (result screen after the last one);
                -- a single level: result screen
                if gsMode new == Campaign
                    then nextStep saved
                    else pure saved { screen = Result True (gsScore new) }
            (InProgress, Loss) -> pure app { screen = Result False (gsScore new) }
            _ -> pure cont
    _ -> pure app

saveWin :: AppState -> GameState -> IO AppState
saveWin app gs 
    | lvlId (gsLevel gs) == Sandbox = pure app
    | otherwise = do 
        let score = Highscore (lvlId (gsLevel gs)) (gsScore gs)
            scores = take 10 (sortBy (flip (comparing hsScore)) (score : highscores app))
            unlocked = max (levelUnlocked app) (nextUnlocked (lvlId (gsLevel gs)))
        res <- try (saveScores scores >> saveProgress unlocked) :: IO (Either IOException ())
        case res of 
            Left err -> pure app { 
                highscores = scores, 
                levelUnlocked = unlocked, 
                message = Just ("Save failed: " ++ show err) }
            Right () -> pure app { highscores = scores, levelUnlocked = unlocked, message = Nothing }  
            
nextUnlocked :: LevelId -> Int
nextUnlocked Level1 = 2 
nextUnlocked Level2 = 3
nextUnlocked RandomVertical = 3
nextUnlocked _ = 1 
