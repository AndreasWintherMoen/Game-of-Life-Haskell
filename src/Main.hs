module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering


window = InWindow "Game of Life" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = play window backgroundColor 30 "" gameAsPicture transformGame (const id)
