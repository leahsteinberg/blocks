module BlockMenu where
import Draggable exposing (..)

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Signal exposing (message)


menuBackground = rect 100 600
                |> filled yellow
                |> move (-300, 0)



makeBlockButtonCustom  : (String, Color, Int) -> Form
makeBlockButtonCustom (str, col, i) =
  let 
      buttonBackground = (color col (centered (fromString str)))
  in
      customButton (Signal.message boxTransform.address (Add str))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (-300, 300 / 10 * (toFloat i))



mapButton = 
    let
        background = color



--filterBackground : Form
--filterBackground =
--  rect 60 30 
--    |> filled lightPurple
--    |> alpha 0.1

--mapBackground : Form
--mapBackground = 
--  rect 60 30
--    |> filled lightRed
--    |> alpha 0.1

--mapBlock : Form
--mapBlock = move (-300, 300) (group [mapBackground, text (fromString "map")] )
--  |> makeClickable (Add "map")

--makeClickable : MetaAction -> Form -> Form
--makeClickable f ma = clickable (Signal.message boxTransform.address ma)

--filterBlock : Form
--filterBlock = move (-300, 260) (group [filterBackground, text (fromString "filter")])
--  |> makeClickable (Add "filter")


--blockMenu : List Form
--blockMenu = [mapBlock, filterBlock]


--main = 
--  collage 700 700 [mapBlock, filterBlock]
