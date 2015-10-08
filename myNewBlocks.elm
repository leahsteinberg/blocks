

import Debug
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (customButton)
import Text exposing (fromString, typeface, style)
import Signal exposing (..)
import Maybe exposing (..)
import List exposing (length)

-- my modules
import View exposing (..)
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing (..)
import BlockMenu exposing (menuButtons)





-- views


applyTypeface = typeface ["Courier New", "times new roman"]


dummyModel : Model
dummyModel =
  let
      filter = (H (Filter {id= 1
                          , form = formHOF 1 "filter" bRed (10, 10)
                          , selected = False, pos = (0, 0)}
                          Nothing (Just {pos =(5000, 5000), rockList = dummyRockList})))
      map = (H (Map {id = 2, form = formHOF 2 "map" bPurple (10, 10), selected = False, pos = (100, 100)} Nothing (Just {pos= (1000, 100), rockList = dummyRockList})))
  in
      {nextID = 2
        , blocks = [filter, map]
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
  collage 700 700 ((flattenForest m.blocks) ++ menuButtons)


main = Signal.map view foldModel

foldModel : Signal Model 
foldModel = Signal.foldp signalRouter dummyModel allUpdateSignals





