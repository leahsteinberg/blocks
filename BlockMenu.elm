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
            |> move (-500, 300 / 10 * (toFloat i))



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
                (els, forms) = expToElsAndForms (H (Map Nothing Nothing)) id
        in 
                {id = id
                    , selected = False
                    , pos = (-200, -200)
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




dummyRockList : List Rock
dummyRockList = [
  {value= 0, solid= True, color = red}
  , {value = 1, solid = False, color = blue}
  , {value = 2, solid = False, color = purple}
  , {value = 3, solid = False, color = red}
  , {value = 4, solid = False, color = purple}
  , {value = 5, solid = True, color = blue}
  , {value = 6, solid = False, color = red}
  , {value = 7, solid = False, color = purple}
  , {value = 8, solid = True, color = red}]


emptyRocksBlock : String -> Color -> BlockTemplate
emptyRocksBlock  str col =

    (\id ->
        let exp = RE (R dummyRockList)
            (els, forms) = expToElsAndForms exp id
        in
              {id= id
                , ele = els
                , selected = False
                , pos= (-(id *20), id*10)
                , exp = exp
                , forms = forms}
                )

menuData = [(emptyFilterBlock, "filter", bRed, 1), (emptyMapBlock, "map", bPurple, 2), (emptyRocksBlock, "rocks", bGreen, 3)]



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
