module Draggable where

import Debug
import Graphics.Input
import Task exposing (..)
import Signal exposing (..)
import Mouse exposing (..)
import Graphics.Element exposing (..)
import Maybe exposing (..)
import Text exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)


-- some code taken from / inspired by : https://github.com/jvoigtlaender/elm-drag-and-drop/blob/master/DragAndDrop.elm


-- MODEL

type alias Model = {nextID: ID
                   , boxes: List (Draggable)
                   }

type alias Draggable = {id : ID
                       , pos : (Float, Float)
                       , boxElement : Element
                       }

type alias ID = Int

type MouseEvent = StartAt (Int, Int) 
                  | MoveFromTo (Int, Int) (Int, Int) 
                  | EndAt (Int, Int) 
                  | NoEvent

type Action = Lift | MoveBy (Int, Int,Int) | Release

type alias DragSignal = Signal ( Action)



boxStrings = ["LOL", "HAHA", "WHAT", "OMG"]
boxElements = List.map makeBox boxStrings

makeBox : String -> Element
makeBox s = putInBox (centered (fromString s))

makeBoxForm : String -> Form
makeBoxForm s = text (fromString s)

-- deal with boxes and hovering
hover = Signal.mailbox Nothing

makeHoverable : Element -> ID -> Element
makeHoverable e id = Graphics.Input.hoverable (Signal.message hover.address << \h -> if h then Just id else Nothing) e

putInBox : Element -> Element
putInBox e =
  let (sx, sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid red)(rect (toFloat sx) (toFloat sy))]]


initModel : Model
initModel = makeDraggables boxElements


makeBoxForModel : Element -> ID -> Draggable
makeBoxForModel e id =
      {id = id
      , pos = (toFloat (id * 25) , toFloat (id* 25))
      , boxElement = makeHoverable e id}


makeDraggables : List (Element) -> Model
makeDraggables boxElementList=
    List.foldr (\el  model ->
              {model | nextID <- model.nextID + 1
              , boxes <- (makeBoxForModel el model.nextID) :: model.boxes
              })
              {nextID = 0, boxes = []}
              boxElementList


moveBy (dx, dy) (x, y) = (x + toFloat dx, y - toFloat dy)


addBox : Model -> Element -> Model
addBox m e =
  {m | 
    nextID  <- m.nextID + 1
    , boxes <- (makeBoxForModel e m.nextID) :: m.boxes}




-- SIGNALS


drag : Maybe (Int) -> MouseEvent -> Maybe (Action)
drag hovering mouseEvent =
  case hovering of
    Just id ->
      case mouseEvent of
       MoveFromTo (ax, ay) (bx, by) -> Just (MoveBy (id, bx - ax, by - ay))
       _ -> Nothing
    _ -> Nothing


isJustAction : Maybe(Action) -> Bool
isJustAction ma =
  case ma of
    Just a -> True
    Nothing -> False

fromJust : Maybe a -> a
fromJust ma =
  case ma of
    Just a -> a

dragSignal : DragSignal
dragSignal =
  let 
      maybeDrag = (Signal.map2 drag hover.signal mouseEvents)
      justDrags = Signal.filter isJustAction (Just Lift) maybeDrag 
      drags = Signal.map fromJust justDrags
  in
      drags



removeNonEvents : MouseEvent -> Bool
removeNonEvents e =
  case e of
    NoEvent -> False
    _ -> True

mouseEvents : Signal MouseEvent
mouseEvents =
  let makeMouseEvent (down, (px, py)) oldEvent =
    case oldEvent of
      StartAt (ix, iy) ->
        if down
        then MoveFromTo (ix, iy) (px, py)
        else EndAt (px, py)

      MoveFromTo (ax, ay) (bx, by) ->
        if down
        then MoveFromTo (bx, by) (px, py)
        else EndAt (px, py)

      EndAt (ix, iy) ->
        if down 
        then StartAt (px, py)
        else NoEvent

      NoEvent ->
        if down
        then StartAt (px, py)
        else NoEvent

  in
     filter removeNonEvents (EndAt (0, 0)) 
       (foldp makeMouseEvent (EndAt (0, 0)) 
       (Signal.map2 (,) Mouse.isDown Mouse.position))



moveBox : Model -> ID -> (Int, Int) -> Model
moveBox m id (dx, dy) =  {m | boxes <-
                                  (List.foldr
                                  (\b boxes ->
                                    if b.id == id
                                    then {b | pos <- moveBy (dx, dy) b.pos} :: boxes
                                    else b :: boxes)
                                                  []
                                                  m.boxes)}

updateDrag : Maybe Action  -> Model -> Model
updateDrag drag m =
        case drag of
          Just (MoveBy (id, dx, dy)) -> moveBox m id (dx, dy)
          _ -> m


drawModel : Model -> Element
drawModel model = 
 collage 700 700 
        (drawBoxes model)

drawBoxes : Model -> List Form
drawBoxes m =
        (List.map (\box -> (Graphics.Collage.move box.pos (toForm box.boxElement))) m.boxes)


