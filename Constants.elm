module Constants where

import Color exposing (rgb, red, blue, black)
import Text exposing (style)
import Graphics.Collage exposing (defaultLine)
import Dict
import Window



hofWidth = 90 + funcWidth
hofHeight = 50
funcWidth = 60
funcHeight = 40
rockListWidth = 10 * rockWidth

rockWidth = 20
rockHeight = hofHeight - 2
blockOffset = 10


-- filter colors
bLightPurple = rgb 204 204 255
bPurple = rgb 127 0 255

filterColor = bLightPurple
predColor = bPurple


--- map colors
bLightGreen = rgb 207 255 213
bGreen = rgb 66 233 150

mapColor = bLightGreen
transformColor = bGreen

bTeal = rgb 169 245 242
--    rgb 88 250 208
bPeriwinkle = rgb 129 159 247

foldColor = bTeal
accumColor = bPeriwinkle

-- other colors
bBlue = rgb 66 233 233
rocksColor = bBlue


rBlue = rgb 1 169 219
rGreen = rgb 128 255 0
rRed = rgb 255 0 64

bOrange = rgb 255 128 0
bYellow = rgb 255 255 153

evalColor = bOrange
rewindColor = bYellow

emptyModel = {nextID = 1, blocks = Dict.empty, dims= (1100, 500), clicked = 0}

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
                        | color <- col, width <- 2.5, dashing <- [4, 4]}

predRockList = [{color = black, value = 0, solid = True},{color = black, value = 0, solid = False}, {color = black, value = 0, solid = True},{color = black, value = 0, solid = False}, {color = rRed, value = 0, solid = True}, {color = rBlue, value = 0, solid = True}, {color = rGreen, value = 0, solid = True}]


    