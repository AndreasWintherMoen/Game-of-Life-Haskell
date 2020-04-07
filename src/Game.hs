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

isCellFilled :: Board -> (Int, Int) -> Bool
isCellFilled board cellCoord = board ! cellCoord

isCellInBounds :: (Int, Int) -> Bool
isCellInBounds (x, y) = x >= 0 && x < cols && y >= 0 && y < rows

isCoordCorrect = inRange ((0, 0), (rows - 1, cols - 1))

initialGame :: Game
initialGame = Game { gameState = Placing
                   , gameBoard = array indexRange $ zip (range indexRange) (repeat False)
                   }
    where indexRange = ((0, 0), (rows, cols))

flipStartCell :: Game -> (Int, Int) -> Game
flipStartCell game cellCoord
    | isCoordCorrect cellCoord = 
        game { gameBoard = board // [(cellCoord, not (isCellFilled board cellCoord))] }
    | otherwise = game
    where board = gameBoard game

switchState :: Game -> Game
switchState game = 
    case gameState game of 
        Placing -> game { gameState = Simulating }
        Simulating -> game { gameState = Placing }

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / fromIntegral cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / fromIntegral cellWidth)
                             )

-- neighborCoords :: Board -> (Int, Int) -> [(Int, Int)]
-- neighborCoords board (x, y) = [ (z, w) | (z, w) <- (board) , isCoordCorrect (z, w)]

sumCells :: [Cell] -> Int
sumCells xs = sum [1 | x <- xs, x] 

numNeighbors :: Board -> (Int, Int) -> Int
numNeighbors board (x, y) = sumCells [board ! (i, j) | i <- [x-1..x+1], j <- [y-1..y+1], isCellInBounds (i, j)]

simulateBoard :: Board -> Board
simulateBoard board = board

simulateStep :: Game -> Game
simulateStep game = game { gameBoard = simulateBoard (gameBoard game) }


transformGame :: Event -> Game -> Game
transformGame (EventKey (SpecialKey KeyEnter) Up _ _) game = switchState game 
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of 
        Placing -> flipStartCell game $ mousePosAsCellCoord mousePos
        Simulating -> game
transformGame _ game = game