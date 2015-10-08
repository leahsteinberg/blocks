module BlockMenu where

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Graphics.Input exposing (customButton)
import Signal exposing (message)

import Model exposing (..)
--import Draggable exposing (..)
import View exposing (formHOF)
import Constants exposing (..)
import SignalProcessing exposing (..)

menuBackground : Form
menuBackground = rect 100 600
                |> filled yellow
                |> move (-300, 0)



makeBlockButtonCustom  : Exp -> String -> Color -> ID -> Form
makeBlockButtonCustom exp str col i =
  let 
      buttonBackground = (color col (centered (fromString str)))
  in
      customButton (Signal.message blockTransform.address  (Add exp))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (-300, 300 / 10 * (toFloat i))



emptyFilterExp : String -> Color -> ID -> Exp
emptyFilterExp str col id = H (Filter {
                                id = id
                                , form = formHOF id str col (-200, 100)
                                , selected = False
                                , pos = (-200, 100) }
                                Nothing Nothing
                                )

emptyMapExp : String -> Color -> ID -> Exp
emptyMapExp str col id = H (Map {
                                id = id
                                , form = formHOF id str col (-100, 0)
                                , selected = False
                                , pos = (-100, 0) }
                                Nothing Nothing
                                )

menuData = [(emptyFilterExp, "filter", bRed, 1), (emptyMapExp, "map", bPurple, 2)]


makeMenuButton : (String -> Color -> ID -> Exp) -> String -> Color -> ID -> Form
makeMenuButton expFunc str col id = 
    makeBlockButtonCustom (expFunc str col id) str col id

menuButtons : List Form
menuButtons = List.map (\(expFunc, str, col, id) -> makeMenuButton expFunc str col id) menuData
    


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
