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


boxStrings = ["LOL", "HAHA", "WHAT", "OMG"]
boxElements = List.map makeBox boxStrings

hover = Signal.mailbox Nothing
makeBox : String -> Element
makeBox s = putInBox (leftAligned (fromString s))

initModel : Model
initModel = makeDraggables boxElements



-- what I need.
-- I need to hold on to a list of the elements. I guess? and I need to map make hoverable over them. I need just one mailbox.

--makeModel : List (String) -> Model
--makeModel boxStringList = makeHoverableBoxes (makeEmptyModelFromStrings boxStringList)


makeDraggables : List (Element) -> Model
makeDraggables boxElementList=
    List.foldr (\el  model ->
              {model | nextID <- model.nextID + 1
              , boxes <- model.boxes ++ [{id = model.nextID 
                                          --, text = el
                                          , pos = (toFloat ((model.nextID) * 75) , toFloat ((model.nextID) * 75))
                                          , boxElement = makeHoverable el model.nextID}]--makeHoverableElement el model.nextID}]
              })
              {nextID = 0, boxes = []}
              boxElementList

--makeHoverableBoxes : Model -> Model
--makeHoverableBoxes model =
--  {model | boxes <- List.map (\el -> {el | boxElement <- makeHoverable (makeBox el.text) el.id}) model.boxes}



makeHoverable : Element -> ID -> Element
makeHoverable e id = Graphics.Input.hoverable (Signal.message hover.address << \h -> if h then Just id else Nothing) e

putInBox : Element -> Element
putInBox e =
  let (sx, sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid red) (rect (toFloat sx) (toFloat sy))]]

moveBy (dx, dy) (x, y) = (x + toFloat dx, y - toFloat dy)

type Action = Lift | MoveBy (Int, Int,Int) | Release


drag : Maybe (Int) -> MouseEvent -> Maybe (Action)
drag hovering mouseEvent =
  case hovering of
    Just id ->
      case mouseEvent of
       MoveFromTo (ax, ay) (bx, by) -> Just (MoveBy (id, bx - ax, by - ay))
       _ -> Nothing
    _ -> Nothing

dragSignal : Signal (Maybe Action)
dragSignal =
  Signal.map2 drag hover.signal mouseEvents

--main =
--  let update m =
--    case m of
--      Just (MoveBy (dx, dy)) -> moveBy (dx, dy)
--      _ -> identity
--  in
--     Signal.map (\p -> collage 700 700  [Graphics.Collage.move p (toForm box)])
--     (foldp update (0, 0) dragSignal)

main = 
  let 
      moveBox m id (dx, dy) = {m | boxes <- 
                                  (List.foldr 
                                  (\b boxes -> if b.id == id then {b | pos <- moveBy (dx, dy) b.pos} :: boxes else b :: boxes)  
                                                  []
                                                  m.boxes)}
      update drag m =
        case drag of
          Just (MoveBy (id, dx, dy)) -> moveBox m id (dx, dy)
          _ -> m
  in
     Signal.map 
        (\model -> collage 700 700 
        (List.map (\box -> (Graphics.Collage.move box.pos (toForm box.boxElement))) model.boxes)) 
        (foldp update initModel (Debug.watch "dragsignal" <~ dragSignal)) 

type MouseEvent = StartAt (Int, Int) | MoveFromTo (Int, Int) (Int, Int) | EndAt (Int, Int) | NoEvent
outNoEvent : MouseEvent -> Bool
outNoEvent e =
  case e of
    NoEvent -> False
    _ -> True

mouseEvents : Signal MouseEvent
mouseEvents =
  let makeMouseEvent (down, (px, py)) oldEvent =
    case oldEvent of
      StartAt (ix, iy) -> if down then MoveFromTo (ix, iy) (px, py) else EndAt (px, py)
      MoveFromTo (ax, ay) (bx, by) -> if down then MoveFromTo (bx, by) (px, py) else EndAt (px, py)
      EndAt (ix, iy) -> if down then StartAt (px, py) else NoEvent
      NoEvent -> if down then StartAt (px, py) else NoEvent
  in
     filter outNoEvent (EndAt (0, 0)) (foldp makeMouseEvent (EndAt (0, 0)) (Signal.map2 (,) Mouse.isDown Mouse.position))


