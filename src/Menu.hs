module Menu ( 
    MenuSelect (..),
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

data MenuSelect = SelectLevel LevelId | StartCampaign
    deriving (Eq)
    
menuW, menuH :: Float
menuW = 360
menuH = 45

menuButtons :: [(MenuSelect, Float)]
menuButtons = [
    (SelectLevel Sandbox, 185),
    (SelectLevel Level1, 125),
    (SelectLevel RandomVertical, 65),
    (SelectLevel Level3, 5),
    (StartCampaign, -55)]

playerBtnW, playerBtnH, playerBtnY :: Float
playerBtnW = 185
playerBtnH = 42
playerBtnY = -225

playerBtn :: [(PlayerSelect, Float)]
playerBtn = [(SinglePlayer, -105), (TwoPlayer, 105)]

--Remove hardcoded variables!!
hitTest :: (Float, Float) -> Maybe MenuSelect
hitTest (mx, my) = listToMaybe
    [ choice 
    | (choice, midY) <- menuButtons
    , abs mx <= menuW / 2
    , abs (my - midY) <= menuH / 2 ]
    
hitTestPlayers :: (Float, Float) -> Maybe PlayerSelect
hitTestPlayers (mx, my) = listToMaybe
    [ select
    | (select, midX) <- playerBtn
    , abs (mx - midX) <= playerBtnW / 2
    , abs (my - playerBtnY) <= playerBtnH / 2]