module BlockMenu where

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Graphics.Input exposing (customButton)
import Signal exposing (message)

import Model exposing (..)
--import Draggable exposing (..)
import View exposing (expToElsAndForms, endForms)
import Constants exposing (..)
import SignalProcessing exposing (..)

menuBackground : Form
menuBackground = rect 100 600
                |> filled yellow
                |> move (-300, 0)



makeMenuButtonCustom  : BlockTemplate -> String -> Color -> Int -> Form
makeMenuButtonCustom blockTemplate str col i =
  let 
      buttonBackground = (color col (centered (fromString str)))
  in
      customButton (Signal.message blockTransform.address (Add blockTemplate))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (-450, 300 / 10 * (toFloat i))



emptyFilterBlock : String -> Color -> BlockTemplate
emptyFilterBlock str col = 
    (\id -> 
        let 
                (els, forms) =expToElsAndForms (H (Filter Nothing Nothing)) id
        in 
                {id = id
                    , selected = False
                    , pos = (-100, -100)
                    , ele = els
                    , exp = H (Filter Nothing Nothing)
                    , forms = forms})
                                

emptyMapBlock : String -> Color -> BlockTemplate
emptyMapBlock str col = 
    (\id -> 
        let 
                (els, forms) =expToElsAndForms (H (Map Nothing Nothing)) id
        in 
                {id = id
                    , selected = False
                    , pos = (100, 100)
                    , ele = els
                    , exp = H (Map Nothing Nothing)
                    , forms = forms})


emptyMapBlock2 : String -> Color -> BlockTemplate
emptyMapBlock2 str col = 
    (\id -> 
        let 
                (els, forms) =expToElsAndForms (H (Map Nothing Nothing)) id
        in 
                {id = id
                    , selected = False
                    , pos = (200, 200)
                    , ele = els
                    , exp = H (Map Nothing Nothing)
                    , forms = forms})

menuData = [(emptyFilterBlock, "filter", bRed, 1), (emptyMapBlock, "map", bPurple, 2)]



makeMenuButton : BlockTemplate -> String -> Color -> Int -> Form
makeMenuButton blockTemp str col i = 
    makeMenuButtonCustom blockTemp str col i

menuButtons : List Form
menuButtons = List.map (\(blockTemp, str, col, i) -> makeMenuButton (blockTemp str col) str col i) menuData
    


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
