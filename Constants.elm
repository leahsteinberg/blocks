module Constants where

import Color exposing (rgb, red)
import Text exposing (style)
import Graphics.Collage exposing (defaultLine)
import Dict



hofWidth = 90 + funcWidth
hofHeight = 50
funcWidth = 60
funcHeight = 40
rockListWidth = 10 * rockWidth

rockWidth = 20
rockHeight = hofHeight - 2
blockOffset = 10

bBlue = rgb 66 233 233
bGreen = rgb 66 233 150
bRed = rgb 231 162 233
bPurple = rgb 207 255 213
evalColor = bPurple
rewindColor = bGreen
filterColor = bBlue
mapColor = bPurple
emptyModel = {nextID = 1, blocks = Dict.empty}

applyStyle =style {
            typeface = ["Courier New"]
          , height = (Just 17.0)
          , color = Color.black
          , bold = True
          , italic = False
          , line = Nothing}


applySmallStyle =style {
            typeface = ["Courier New"]
          , height = (Just 8.0)
          , color = Color.black
          , bold = True
          , italic = False
          , line = Nothing}


floatPos pos = (toFloat (fst pos), toFloat (snd pos))


lineStyle col = {defaultLine
                | color <- col, width <- 10.0}

dashedLineStyle col = {defaultLine
                        | color <- col, width <- 2.5, dashing <- [5, 3]}


    