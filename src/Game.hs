module Game where

import Data.Array
import Graphics.Gloss.Interface.Pure.Game

data State = Placing | Simulating
type Cell = Maybe Int
type Board = Array (Int, Int) Cell

data Game = Game { gameState :: State
                 , gameBoard :: Board
                 }


cols :: Int
cols = 30
rows :: Int
rows = 30

initialGame :: Game
initialGame = Game { gameState = Placing
                   , gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   }
    where indexRange = ((0, 0), (rows, cols))

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of 
        Placing -> game
        Simulating -> game
transformGame _ game = game