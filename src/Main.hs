module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Time.Clock

import Game
import Rendering


window = InWindow "Game of Life" (screenWidth, screenHeight) (100, 100)

main :: IO ()
main = do
    t0 <- getCurrentTime
    play window Rendering.backgroundColor 30 (Game.initialGame t0) Rendering.gameAsPicture Game.transformGame Game.simulateGame


