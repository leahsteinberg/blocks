

import Debug
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (customButton)
import Text exposing (fromString, typeface, style)
import Signal exposing (..)
import Maybe exposing (..)
import List exposing (length)
import Dict

-- my modules
import View exposing (..)
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing (..)
import BlockMenu exposing (menuButtons)





-- views


applyTypeface = typeface ["Courier New", "times new roman"]


--dummyModel : Model
--dummyModel =
--  let
--      filter = (H (Filter {id= 1
--                          , form = formHOF 1 "filter" bRed (10, 10)
--                          , selected = False, pos = (0, 0)}
--                          Nothing (Just {pos =(5000, 5000), rockList = dummyRockList})))
--      map = (H (Map {id = 2, form = formHOF 2 "map" bPurple (10, 10), selected = False, pos = (100, 100)} Nothing (Just {pos= (1000, 100), rockList = dummyRockList})))
--  in
--      {nextID = 2
--        , blocks = [filter, map]
--        }

dummyBlocks = Dict.insert 1 dummyMapAndRockBlock Dict.empty
dummyModel2 = {nextID = 2, blocks = dummyBlocks}

dummyMapAndRockBlock = 
  let 
  expression = H (Map Nothing (Just (Higher (Filter Nothing (Just (Higher (Map  Nothing (Just (R dummyRockList)) )))))))
  (els, forms) = expToElsAndForms expression 1
  in
      {id = 1
      , ele = els
      , pos = (0, 0)
      , exp = expression
      , forms = forms
      , selected = False}


dummyRockBlock = 
  let (els, forms) = expToElsAndForms (RE (R  dummyRockList)) 1
  in
      {id= 1
          , ele = els
          , selected = False
          , pos= (100, 100)
          , exp = RE (R dummyRockList)
          , forms = forms
           }


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

--main = collage 700 700 (viewRocks (Just {pos= (-300, 300), rockList= dummyRockList} ))

helperCircles = List.map (\ p -> (move  p (filled green(circle 5.0)))) [(0.0, 0.0)
                                                                        , (-50.0, 0.0)
                                                                        , (50.0, 0.0)
                                                                        , (-100.0, 0.0)
                                                                        , (100.0, 0.0)
                                                                        , (-200.0, 0.0)
                                                                        , (200.0, 0.0)
                                                                        , (0.0, 100.0)
                                                                        , (0.0, -100.0)
                                                                        , (-300.0, 0.0)
                                                                        , (300, 0.0)]

view : Model -> Element
view m =
  let blockList = Dict.values m.blocks
  in
      collage 1000 700
      ( 
        (displayForms blockList) ++ 
         menuButtons ++
        (displayElements blockList) )


displayElements : List Block -> List Form
displayElements blocks =
  List.concatMap (\b-> 
    List.map (\e ->  move (floatPos b.pos) (toForm e)) b.ele
   ) blocks


displayForms : List Block -> List Form
displayForms blocks = 
  List.concatMap (\b -> 
    List.map (\f -> move (floatPos b.pos) f) b.forms) blocks

--displayBlock : Block -> List Form 
--displayBlock b =
--  (List.map (\f -> move (floatPos b.pos) f) b.forms) ++ [( move (floatPos b.pos) (toForm b.ele))]

main = Signal.map view (Debug.watch "modelle" <~ foldModel)

foldModel : Signal Model 
foldModel = Signal.foldp signalRouter dummyModel2 allUpdateSignals

