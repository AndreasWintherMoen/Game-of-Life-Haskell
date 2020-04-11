module Rendering where

import Data.Array
import Graphics.Gloss
import Graphics.UI.GLUT.Fonts

import Game

backgroundColor = makeColor 0 0 0 255
boardGridColor = makeColorI 255 255 255 255
filledCellColor = makeColorI 240 130 130 255
buttonColor = makeColorI 200 180 180 55
textColor = makeColorI 100 50 255 255

boardAsPicture :: Board -> Picture
boardAsPicture board = pictures [ color boardGridColor $ boardGrid
                            , color filledCellColor $ filledCellsOfBoard board
                            , ui
                            ]

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * fromIntegral cellWidth + fromIntegral cellWidth * 0.5
          y = fromIntegral row * fromIntegral cellHeight + fromIntegral cellHeight * 0.5

button :: [Char] -> (Int, Int) -> Picture
button text (width, height) = pictures [
      color buttonColor $ rectangleSolid (fromIntegral width) (fromIntegral height)
    , color textColor $ translate (-40.0) (-15.0) $ Scale 0.3 0.3 $ Text text
    ]

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
                              , (i * fromIntegral cellWidth, fromIntegral (screenHeight - marginTop))
                              ]
                       , line [ (0.0,                      i * fromIntegral cellHeight)
                              , (fromIntegral screenWidth, i * fromIntegral cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral Game.rows]

ui :: Picture
ui = translate x y $ button "start" (100, 50)
    where x = (fromIntegral screenWidth) * 0.5
          y = (fromIntegral screenHeight) - (fromIntegral marginTop) * 0.5


gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5) )
                               frame
    where frame = boardAsPicture (gameBoard game)