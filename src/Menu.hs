module Menu ( 
    menuButtons,
    menuW,
    menuH,
    hitTest,
    playerBtn,
    playerBtnW,
    playerBtnH,
    playerBtnY,
    hitTestPlayers
  ) where

import Data.Maybe (listToMaybe)
import GameData (LevelId (..), PlayerSelect (..))

menuW, menuH :: Float
menuW = 360
menu H = 50

menuButtons :: [(LevelId, Float)]
menuButtons = 
    [ (Sandbox,  150)
    , (Level1,    50)
    , (Level2,   -50)
    , (Level3,  -150)]

playerBtnW, playerBtnH, playerBtnY :: Float
playerBtnW = 185
playerBtnH = 42
playerBtnY = -220

playerBtn :: [(PlayerSelect, Float)]
playerBtn = [(SinglePlayer, -105), (TwoPlayer, 105)]

--Remove hardcoded variables!!
hitTest :: (Float, Float) -> Maybe LevelId
hitTest (mx, my) = listToMaybe
    [ lvl 
    | (level, midY) <- menuButtons
    , abs mx <= menuW / 2
    , abs (my - midY) <= menuH / 2 ]
    
hitTestPlayers :: (Float, Float) -> Maybe PlayerSelect
hitTestPlayers (mx, my) = listToMaybe
    [ select
    | (select midX) <- playerBtn
    , abs (mx - midX) <= playerBtnW / 2
    , abs (my - playerBtnY) <= playerBtnH / 2]