module Rendering where

import Data.Array
import Graphics.Gloss

import Game

backgroundColor = makeColor 0 0 0 255
boardGridColor = makeColorI 255 255 255 255
filledCellColor = makeColorI 240 130 130 255

boardAsPicture :: Board -> Picture
boardAsPicture board = pictures [ color boardGridColor $ boardGrid
                            , color filledCellColor $ filledCellsOfBoard board
                            ]

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * fromIntegral cellWidth + fromIntegral cellWidth * 0.5
          y = fromIntegral row * fromIntegral cellHeight + fromIntegral cellHeight * 0.5

filledCell :: Picture
filledCell = rectangleSolid (fromIntegral cellWidth) (fromIntegral cellHeight)

filledCellsOfBoard :: Board -> Picture
filledCellsOfBoard board = 
    pictures
    $ map (snapPictureToCell filledCell . fst)
    $ filter (\(_, e) -> e)
    $ assocs board

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