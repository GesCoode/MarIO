module Main where

import Graphics.Gloss (Display (..), Picture, black, color, loadBMP, rectangleSolid, white)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Directory (doesFileExist)

import Data.List (sortBy)
import Data.Ord (comparing, Down (..))

import Controller (handleEvent, stepApp)
import GameData (AppState (..), Highscore (..), SpriteAtlas, initialApp)
import Persistence (loadScores)
import View (drawApp)

fps :: Int
fps = 60

loadSprite :: String -> FilePath -> IO (String, Picture)
loadSprite name path = do
    exists <- doesFileExist path
    pic <- if exists
        then loadBMP path
        else pure (color white (rectangleSolid 120 80))
    pure (name, pic)

-- Loading all sprites
loadSprites :: IO SpriteAtlas
loadSprites = mapM (\n -> loadSprite n ("Assets/" ++ n ++ ".bmp")) allSprites
    where
        allSprites = concat [tileSprites, backgroundSprites, heartSprites, bossSprites,
                             characterSprites, broSprites, goombaSprites]

-- Level textures
tileSprites :: [String]
tileSprites = ["Ground", "Square", "Block", "Platform"]

-- Level backgrounds
backgroundSprites :: [String]
backgroundSprites = ["1-1-Background", "Clouds", "BossFightBackground"]

-- Health display.
heartSprites :: [String]
heartSprites = ["FullHeart", "EmptyHeart"]

-- Boss: BT0/BT1 about to act or jumping, BTN neutral/hurt.
bossSprites :: [String]
bossSprites = ["BT0", "BT1", "BTN"]

-- Hammer Bro idle/throw poses and the hammer frames
broSprites :: [String]
broSprites = ["HI0", "HI1", "HT"] ++ ["H" ++ show i | i <- [0 .. 6 :: Int]]

-- Goomba walk cycle
goombaSprites :: [String]
goombaSprites = ["GW0", "GW1", "GW2", "GF", "GD"]

characterSprites :: [String]
characterSprites =
    [ c : rest
    | c <- "ML"
    , rest <- ["D", "JL", "JR"] ++ [d : show i | d <- "LR", i <- [0 .. 2 :: Int]]
    ]

-- Main's main
main :: IO ()
main = do
    atlas <- loadSprites
    scores <- loadScores
    let app0 = (initialApp atlas) {
            highscores = sortBy (comparing (Down . hsScore)) (either (const []) id scores) }
    playIO
        (InWindow "MarIO" (800, 600) (100, 100))
        black
        fps
        app0
        drawApp
        handleEvent
        stepApp
