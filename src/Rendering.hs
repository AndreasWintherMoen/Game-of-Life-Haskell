module Rendering where

import Data.Array
import Graphics.Gloss

import Game

backgroundColor = makeColor 0 0 0 255
boardGridColor = makeColorI 255 255 255 255

cellWidth :: Int
cellWidth = 20
cellHeight :: Int
cellHeight = 20

screenWidth :: Int
screenWidth = cellWidth * Game.cols
screenHeight :: Int
screenHeight = cellHeight * Game.rows


boardAsPicture :: Board -> Picture
boardAsPicture _ = pictures [ color boardGridColor $ boardGrid ]

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * fromIntegral cellWidth, 0.0)
                              , (i * fromIntegral cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * fromIntegral cellHeight)
                              , (fromIntegral screenWidth, i * fromIntegral cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral Game.rows]

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = boardAsPicture (gameBoard game)