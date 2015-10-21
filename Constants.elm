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


-- other colors
bBlue = rgb 66 233 233
rocksColor = bBlue

bOrange = rgb 255 128 0
bYellow = rgb 255 255 153

evalColor = bOrange
rewindColor = bYellow






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
                        | color <- col, width <- 2.5, dashing <- [4, 4]}


    