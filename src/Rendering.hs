module Rendering where

import Data.Array
import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255

boardAsPicture board = pictures [ color boardGridColor $ boardGrid ]

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral rows]

gameAsPicture board = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = boardAsPicture board