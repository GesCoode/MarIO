module Main where

import Graphics.Gloss (Display (..), Picture, black, color, loadBMP, rectangleSolid, white)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Directory (doesFileExist)

import Controller (handleEvent, stepApp)
import GameData (SpriteAtlas, initialApp)
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

loadSprites :: IO SpriteAtlas
loadSprites = do
  mapM (\n -> loadSprite n ("assets/" ++ n ++ ".bmp")) ("block" : characterSprites)

-- For M (Mario) and L (Luigi): death, jump left/right, walk left/right frames 0-2.
characterSprites :: [String]
characterSprites =
  [ c : rest
  | c <- "ML"
  , rest <- ["D", "JL", "JR"] ++ [d : show i | d <- "LR", i <- [0 .. 2 :: Int]]
  ]

main :: IO ()
main = do
  atlas <- loadSprites
  playIO
    (InWindow "MarIO" (800, 600) (100, 100))
    black
    fps
    (initialApp atlas)
    drawApp
    handleEvent
    stepApp
