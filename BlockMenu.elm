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



makeMenuButtonCustom  : ExpTemplate -> String -> Color -> Int -> Form
makeMenuButtonCustom expTemplate str col i =
  let 
      buttonBackground = (color col (centered (fromString str)))
  in
      customButton (Signal.message blockTransform.address (Add expTemplate))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (-300, 300 / 10 * (toFloat i))



emptyFilterExp : String -> Color -> ExpTemplate
emptyFilterExp str col = (\id -> H (Filter {
                                id = id
                                , form = formHOF id str col (-10, 0)
                                , selected = False
                                , pos = (-10, 0) }
                                Nothing Nothing
                                ))

emptyMapExp : String -> Color -> ExpTemplate
emptyMapExp str col = (\id -> H (Map {
                                id = id
                                , form = formHOF id str col (-100, 200)
                                , selected = False
                                , pos = (-100, 200) }
                                Nothing Nothing
                                ))
emptyMapExp2 : String -> Color -> ExpTemplate
emptyMapExp2 str col = (\id -> H (Map {
                                id = id
                                , form = formHOF id str col (0, 100)
                                , selected = False
                                , pos = (0, 100) }
                                Nothing Nothing
                                ))

menuData = [(emptyFilterExp, "filter", bRed, 1), (emptyMapExp, "map", bPurple, 2), (emptyMapExp2, "map2", bRed, 3)]



makeMenuButton : ExpTemplate -> String -> Color -> Int -> Form
makeMenuButton expTemp str col i = 
    makeMenuButtonCustom expTemp str col i

menuButtons : List Form
menuButtons = List.map (\(expFunc, str, col, i) -> makeMenuButton (expFunc str col) str col i) menuData
    


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
