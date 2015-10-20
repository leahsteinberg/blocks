

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
import Window

-- my modules
--import View exposing (..)
import ViewFragment exposing (fragmentToForms)
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing (..)
import Color exposing (red, black)
import BlockMenu exposing (menuButtons)
import ControlPanel exposing (evalButton)


applyTypeface = typeface ["Courier New", "times new roman"]


toRedFunc : Rock -> Rock
toRedFunc rock =
    {rock | color <- red}




--dummyBlocksSmall = Dict.insert 3 (dummyRockBlock 3 )(Dict.insert 2 (dummyMapAndRockBlock onlyFilterExp 2) (Dict.insert 1 (dummyMapAndRockBlock onlyMapExpwFunc 1) Dict.empty))

--dummyBlocksSmall2 = Dict.insert 2 (dummyMapAndRockBlock onlyFilterExp 2) (Dict.insert 1 (dummyMapAndRockBlock onlyMapExp 1) Dict.empty)

--onlyMapExpwFunc = H (Map (Just (T toRedFunc)) Nothing)


--dummyModelSmall2 = {nextID = 4, blocks = dummyBlocksSmall2}


--dummyBlocks = Dict.insert 4 (dummyMapAndRockBlock smallerExp 4) (Dict.insert 3 (dummyRockBlock 3) (Dict.insert 2 (dummyMapAndRockBlock smallExp 2) (Dict.insert 1 (dummyMapAndRockBlock bigExp 1) Dict.empty)))
--dummyModel2 = {nextID = 5, blocks = dummyBlocks}

--dummyModelSmall = {nextID = 4, blocks = dummyBlocksSmall}

--onlyFilterExp = H (Filter Nothing Nothing)

--onlyMapExp = H (Map  Nothing Nothing)

--smallerExp = H (Map  Nothing (Just (R dummyRockList)))

--bigExp = H (Map Nothing (Just (Higher (Filter Nothing (Just (Higher (Map  Nothing Nothing )))))))

--smallExp = H (Filter Nothing (Just (Higher (Map  Nothing (Just (R dummyRockList)) ))))

--dummyMapAndRockBlock exp id = 
--  let 
--  (els, forms) = expToElsAndForms exp id
--  in
--      {id = id
--      , ele = els
--      , pos = (-(id*50), -(id*50))
--      , exp = exp
--      , forms = forms
--      , selected = False}


--dummyRockBlock id = 
--  let 

--    (els, forms) = expToElsAndForms (E (R  dummyRockList)) id
--  in
--      {id= id
--          , ele = els
--          , selected = False
--          , pos= (0, 0)
--          , exp = RE (R dummyRockList)
--          , forms = forms
--           }


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

emptyModel = {nextID = 1, blocks = Dict.empty}

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


centerBall = circle 10.0
              |> filled  purple
              |> move (0,0)



view : (Int, Int) -> Model -> Element
view (w, h) m =

  let blockList = Dict.values m.blocks
  in
      collage w h
      (  centerBall :: evalButton::
        (displayForms blockList) ++ 
          (displayElements blockList) ++
         (menuButtons w)
         )


displayElements : List Block -> List Form
displayElements blocks =
  List.concatMap (\b-> 
    List.map (\e ->  move (floatPos b.pos) e) b.ele) blocks


displayForms : List Block -> List Form
displayForms blocks = 
  List.concatMap (\b -> 
    List.map (\f -> move (floatPos b.pos) f) b.forms) blocks


main = Signal.map2 view Window.dimensions foldModel

foldModel : Signal Model 
foldModel = Signal.foldp signalRouter emptyModel allUpdateSignals

