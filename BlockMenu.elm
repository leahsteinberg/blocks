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
            |> move (xValue, 300 / 10 * (toFloat i) + 10 )


makeFuncButton : BlockTemplate ->  Int -> Int -> Color ->Form
makeFuncButton blockTemplate i width col =
    let
        xValue =  -((toFloat width)/2 - 30)
        yValue = 300 / 10 * (toFloat i) + 10
        fakeFuncBlock = blockTemplate -1
        fakeFuncFrag = fakeFuncBlock.exp
        buttonBackgroundForms = fst (fragmentToForms  fakeFuncFrag -1)
        buttonElement = collage (rockWidth*3) (rockWidth*3) buttonBackgroundForms
    in
        customButton (Signal.message blockTransform.address (Add blockTemplate))
            buttonElement buttonElement buttonElement
                |> toForm
                |> move (xValue, yValue)



menuData : List ((String -> Color -> BlockTemplate), String, Color, Int)
menuData = [(emptyFilterBlock, "filter", filterColor, -1)
            , (emptyMapBlock, "map", mapColor, 6)
            , (emptyRocksBlock, "rocks", rocksColor, 7)
            , (emptyFoldBlock, "fold", foldColor, 8)]

transformMenuData : List (BlockTemplate, Int, Color)
transformMenuData = [(emptyTransformBlock (toColor rRed), 5, transformColor)
                , (emptyTransformBlock (toColor rBlue), 4, transformColor)
                , (emptyTransformBlock (toColor rGreen), 3, transformColor)
                , (emptyTransformBlock (toSolid True), 2, transformColor)
                , (emptyTransformBlock (toSolid False), 1, transformColor)
                , (emptyTransformBlock (addValueToRock 2), 0, transformColor)]


predMenuData : List (BlockTemplate, Int, Color)
predMenuData =   [(emptyPredBlock (onlyColor rRed), -3, predColor)
                , (emptyPredBlock (onlyColor rBlue), -4, predColor)
                , (emptyPredBlock (onlyColor rGreen), -5, predColor)
                , (emptyPredBlock (onlySolid True), -6, predColor)
                , (emptyPredBlock (onlySolid False), -7, predColor)]

menuButtons width = makeMenuButtons width ++ makeFuncButtons width predMenuData ++ makeFuncButtons width transformMenuData

makeMenuButtons : Int -> List Form
makeMenuButtons width = List.map (\(blockTemp, str, col, i) -> makeMenuButtonCustom (blockTemp str col) str col i width) menuData
    
makeFuncButtons : Int -> List (BlockTemplate, Int, Color) -> List Form
makeFuncButtons width funcList = List.map (\(blockTemp, i, col) -> makeFuncButton blockTemp i width col) funcList


-- - - - - - - - - - F U N C - B L O C K S  - - - - - - - - - -      

transformFragment : (Rock -> Rock) -> Fragment
transformFragment func = 
      F (T func) 

toColor : Color -> Rock -> Rock
toColor col rock = {rock | color <- col}

--toRedFunc : Rock -> Rock
--toRedFunc rock = {rock | color <- rRed}

--toBlueFunc : Rock -> Rock
--toBlueFunc rock = {rock | color <- rBlue}

toSolid : Bool -> Rock -> Rock
toSolid solid rock = {rock | solid <- solid}

addValueToRock : Int -> Rock -> Rock
addValueToRock num rock = {rock | value <- rock.value + num}


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
                , pos= (-((id *30)%500), (id*10)%500)
                , exp = fragment
                , forms = forms}
            )


predFragment : (Rock -> Bool) -> Fragment
predFragment func =
    F (P func)


onlyColor : Color -> Rock -> Bool
onlyColor col rock = rock.color == col

--onlyRedFunc : Rock -> Bool
--onlyRedFunc rock = rock.color == rRed

--onlyBlueFunc : Rock -> Bool
--onlyBlueFunc rock = rock.color == rBlue

--onlyGreenFunc : Rock -> Bool
--onlyGreenFunc rock = rock.color

onlySolid : Bool -> Rock -> Bool
onlySolid yes rock = rock.solid == yes 



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
                , pos= (-((id *30)% 500) , (id*10)%500)
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

emptyFoldBlock : String -> Color -> BlockTemplate
emptyFoldBlock str col = 
    (\id -> 
        let 
                fragment = (H (Fold Nothing []))
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
  {value= 0, solid= True, color = rRed}
  , {value = 1, solid = False, color = rBlue}
  , {value = 2, solid = False, color = rGreen}
  , {value = 3, solid = False, color = rRed}
  , {value = 4, solid = False, color = rGreen}
  , {value = 0, solid = True, color = rBlue}
  , {value = 6, solid = False, color = rRed}
  , {value = 2, solid = False, color = rGreen}]


emptyRocksBlock : String -> Color -> BlockTemplate
emptyRocksBlock  str col =

    (\id ->
        let fragment = E (R dummyRockList)
            (els, forms) = fragmentToForms fragment id
        in
              {id= id
                , ele = els
                , selected = False
                , pos= (-((id *20)%500), (id*10)%500)
                , exp = fragment
                , forms = forms}
                )


