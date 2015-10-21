module BlockMenu where

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Graphics.Input exposing (customButton)
import Signal exposing (message)

import Model exposing (..)
--import Draggable exposing (..)
import ViewFragment exposing (fragmentToForms)
--import View exposing (fragmentToForms, endForms)
import Constants exposing (..)
import SignalProcessing exposing (..)
import Debug

menuBackground : Form
menuBackground = rect 100 600
                |> filled yellow
                |> move (-300, 0)



makeMenuButtonCustom  : BlockTemplate -> String -> Color -> Int -> Int -> Form
makeMenuButtonCustom blockTemplate str col i width =
  let 
      buttonBackground = (color col (centered (fromString str)))
      xValue =  -((toFloat width)/2 - 30)
  in
      customButton (Signal.message blockTransform.address (Add blockTemplate))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (xValue, 300 / 10 * (toFloat i))


makeFuncButton : BlockTemplate ->  Int -> Int -> Color ->Form
makeFuncButton blockTemplate i width col =
    let
        xValue =  -((toFloat width)/2 - 30)
        yValue = 300 / 10 * (toFloat i)
        fakeFuncBlock = blockTemplate 0
        fakeFuncFrag = fakeFuncBlock.exp
        buttonBackgroundForms = fst (fragmentToForms  fakeFuncFrag -1)
        buttonElement = collage (rockWidth*3) (rockWidth*3) buttonBackgroundForms
    in
        customButton (Signal.message blockTransform.address (Add blockTemplate))
            buttonElement buttonElement buttonElement
                |> toForm
                |> move (xValue, yValue)

        



emptyFilterBlock : String -> Color -> BlockTemplate
emptyFilterBlock str col = 
    (\id -> 
        let     
                fragment = (H (Filter Nothing []))
                (els, forms) = fragmentToForms fragment id
        in 
                {id = id
                    , selected = False
                    , pos = (-100, -100)
                    , ele = els
                    , exp = fragment
                    , forms = forms})
                                
emptyMapBlock : String -> Color -> BlockTemplate
emptyMapBlock str col = 
    (\id -> 
        let 
                fragment = (H (Map Nothing []))
                (els, forms) = fragmentToForms fragment id
        in 
                {id = id
                    , selected = False
                    , pos = (-200, -200)
                    , ele = els
                    , exp = fragment
                    , forms = forms})

transformFragment : (Rock -> Rock) -> Fragment
transformFragment func = 
      F (T func) 


toRedFunc : Rock -> Rock
toRedFunc rock = {rock | color <- red}

toBlueFunc : Rock -> Rock
toBlueFunc rock = {rock | color <- blue}



emptyTransformRedBlock :  BlockTemplate
emptyTransformRedBlock =
  (\id ->
    let
        fragment = transformFragment toRedFunc
        (els, forms)  = fragmentToForms fragment id
    in
                  {id= id
                , ele = els
                , selected = False
                , pos= (-(id *30), id*10)
                , exp = fragment
                , forms = forms}
            )


emptyTransformBlueBlock :  BlockTemplate
emptyTransformBlueBlock =
  (\id ->
    let
        fragment = transformFragment toBlueFunc
        (els, forms)  = fragmentToForms fragment id
    in
                  {id= id
                , ele = els
                , selected = False
                , pos= (-(id *30), id*10)
                , exp = fragment
                , forms = forms}
            )

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
        let fragment = E (R dummyRockList)
            (els, forms) = fragmentToForms fragment id
        in
              {id= id
                , ele = els
                , selected = False
                , pos= (-(id *20), id*10)
                , exp = fragment
                , forms = forms}
                )

menuData : List ((String -> Color -> BlockTemplate), String, Color, Int)
menuData = [(emptyFilterBlock, "filter", filterColor, 1), (emptyMapBlock, "map", mapColor, 2), (emptyRocksBlock, "rocks", rocksColor, 3)]

funcMenuData : List (BlockTemplate, Int, Color)
funcMenuData = [(emptyTransformRedBlock, -1, transformColor), (emptyTransformBlueBlock, -3, transformColor)]


menuButtons width = makeMenuButtons width ++ makeFuncButtons width

makeMenuButtons : Int -> List Form
makeMenuButtons width = List.map (\(blockTemp, str, col, i) -> makeMenuButtonCustom (blockTemp str col) str col i width) menuData
    
makeFuncButtons : Int -> List Form
makeFuncButtons width  = List.map (\(blockTemp, i, col) -> makeFuncButton blockTemp i width col) funcMenuData

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
