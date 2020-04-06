module Game where

import Data.Array

data State = Placing | Simulating
type Cell = Maybe Int
type Board = Array (Int, Int) Cell

cols :: Int
cols = 30
rows :: Int
rows = 30

cellWidth :: Float
cellWidth = 20.0
cellHeight :: Float
cellHeight = 20.0

screenWidth :: Int
screenWidth = round cellWidth * cols
screenHeight :: Int
screenHeight = round cellHeight * rows



initialGame :: Board
initialGame = array indexRange $ zip (range indexRange) (repeat Nothing)
    where indexRange = ((0, 0), (rows, cols))

transformGame _ game = game