module Game where

import Data.Array
import Graphics.Gloss.Interface.Pure.Game


data State = Placing | Simulating
type Cell = Bool
type Board = Array (Int, Int) Cell

data Game = Game { gameState :: State
                 , gameBoard :: Board
                 }


cols :: Int
cols = 30
rows :: Int
rows = 30

cellWidth :: Int
cellWidth = 20
cellHeight :: Int
cellHeight = 20

screenWidth :: Int
screenWidth = cellWidth * Game.cols
screenHeight :: Int
screenHeight = cellHeight * Game.rows


isCoordCorrect = inRange ((0, 0), (rows - 1, cols - 1))

initialGame :: Game
initialGame = Game { gameState = Placing
                   , gameBoard = array indexRange $ zip (range indexRange) (repeat False)
                   }
    where indexRange = ((0, 0), (rows, cols))

flipStartCell :: Game -> (Int, Int) -> Game
flipStartCell game cellCoord
    | isCoordCorrect cellCoord = 
        game { gameBoard = board // [(cellCoord, True)] }
    | otherwise = game
    where board = gameBoard game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / fromIntegral cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / fromIntegral cellWidth)
                             )


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of 
        Placing -> flipStartCell game $ mousePosAsCellCoord mousePos
        Simulating -> game
transformGame _ game = game