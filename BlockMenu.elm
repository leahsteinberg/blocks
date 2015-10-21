module BlockMenu where

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Graphics.Input exposing (customButton)
import Signal exposing (message)

import Model exposing (..)
import ViewFragment exposing (fragmentToForms)
import Constants exposing (..)
import SignalProcessing exposing (..)
import Debug


makeMenuButtonCustom  : BlockTemplate -> String -> Color -> Int -> Int -> Form
makeMenuButtonCustom blockTemplate str col i width =
  let 
      buttonBackground = (color col (centered (fromString str)))
      xValue =  -((toFloat width)/2 - 30)
  in
      customButton (Signal.message blockTransform.address (Add blockTemplate))
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (xValue, 300 / 10 * (toFloat i) )


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



menuData : List ((String -> Color -> BlockTemplate), String, Color, Int)
menuData = [(emptyFilterBlock, "filter", filterColor,-3)
            , (emptyMapBlock, "map", mapColor, 7)
            , (emptyRocksBlock, "rocks", rocksColor, 8)]

transformMenuData : List (BlockTemplate, Int, Color)
transformMenuData = [(emptyTransformBlock toRedFunc, 5, transformColor)
                , (emptyTransformBlock toBlueFunc, 3, transformColor)
                , (emptyTransformBlock (toSolid True), 1, transformColor)
                , (emptyTransformBlock (toSolid False), -1, transformColor)]


predMenuData : List (BlockTemplate, Int, Color)
predMenuData =   [(emptyPredBlock onlyRedFunc, -5, predColor)
                , (emptyPredBlock onlyBlueFunc, -7, predColor)]

menuButtons width = makeMenuButtons width ++ makeFuncButtons width predMenuData ++ makeFuncButtons width transformMenuData

makeMenuButtons : Int -> List Form
makeMenuButtons width = List.map (\(blockTemp, str, col, i) -> makeMenuButtonCustom (blockTemp str col) str col i width) menuData
    
makeFuncButtons : Int -> List (BlockTemplate, Int, Color) -> List Form
makeFuncButtons width funcList = List.map (\(blockTemp, i, col) -> makeFuncButton blockTemp i width col) funcList


-- - - - - - - - - - F U N C - B L O C K S  - - - - - - - - - -      

transformFragment : (Rock -> Rock) -> Fragment
transformFragment func = 
      F (T func) 

toRedFunc : Rock -> Rock
toRedFunc rock = {rock | color <- red}

toBlueFunc : Rock -> Rock
toBlueFunc rock = {rock | color <- blue}

toSolid : Bool -> Rock -> Rock
toSolid solid rock = {rock | solid <- solid}


emptyTransformBlock : (Rock -> Rock) -> BlockTemplate
emptyTransformBlock func =
  (\id ->
    let
        fragment = transformFragment func
        (els, forms)  = fragmentToForms fragment id
    in
                  {id= id
                , ele = els
                , selected = False
                , pos= (-(id *30), id*10)
                , exp = fragment
                , forms = forms}
            )


predFragment : (Rock -> Bool) -> Fragment
predFragment func =
    F (P func)


onlyRedFunc : Rock -> Bool
onlyRedFunc rock = rock.color == red

onlyBlueFunc : Rock -> Bool
onlyBlueFunc rock = rock.color == blue





emptyPredBlock :  (Rock -> Bool) -> BlockTemplate
emptyPredBlock func =
  (\id ->
    let
        fragment = predFragment func
        (els, forms)  = fragmentToForms fragment id
    in
                  {id= id
                , ele = els
                , selected = False
                , pos= (-(id *30), id*10)
                , exp = fragment
                , forms = forms}
            )



-- - - - - - - - - - H O F - B L O C K S  - - - - - - - - - -      

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




-- - - - - - - - - - R O C K S - B L O C K S  - - - - - - - - - -   
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


