module Constants where

import Color exposing (rgb)
import Text exposing (style)




hofWidth = 70 + funcWidth
hofHeight = 50
funcWidth = 70
funcHeight = 40
rockListWidth = 10 * rockWidth
rockWidth = 20
rockHeight = 40

bBlue = rgb 66 233 233
bGreen = rgb 66 233 150
bRed = rgb 231 162 233
bPurple = rgb 207 169 213


applyStyle =style {
            typeface = ["Courier New"]
          , height = (Just 11.0)
          , color = Color.black
          , bold = True
          , italic = False
          , line = Nothing}

floatPos pos = (toFloat (fst pos), toFloat (snd pos))





