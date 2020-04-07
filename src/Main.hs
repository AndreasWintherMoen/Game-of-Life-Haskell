module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering


window = InWindow "Game of Life" (screenWidth, screenHeight) (100, 100)

main :: IO ()
main = play window Rendering.backgroundColor 30 Game.initialGame Rendering.gameAsPicture Game.transformGame (const id)

