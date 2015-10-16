module Drag where
import Mouse
import Debug
import Graphics.Input
import Graphics.Element exposing (Element)
import Model exposing (..)
import Signal exposing (..)

-- - - - - - - MODEL - - - - - - - -- - - - - - -

type MouseEvent = StartAt (Int, Int) 
                  | MoveFromTo (Int, Int) (Int, Int) 
                  | EndAt (Int, Int) 
                  | NoEvent

hover = Signal.mailbox Nothing

-- - - - - - - - A P I - - -- - - - - - - - - - -

makeHoverable : ID -> Element -> Element
makeHoverable id e = Graphics.Input.hoverable (Signal.message hover.address << \h -> if h then Just id else Nothing) e

dragSignal : Signal DragAction
dragSignal =
  let 
      maybeDrag = Signal.map2 createDrag  hover.signal mouseEvents
      justDrags = Signal.filter isJustAction (Just Lift) maybeDrag 
      drags = Signal.map fromJust justDrags
  in
      drags

-- - - - - - S I G N A L - M A N I P U L A T I O N - - - - -

createDrag : Maybe Int -> MouseEvent -> Maybe DragAction
createDrag hovering mouseEvent =
  case hovering of
    Just id ->
      case mouseEvent of
       MoveFromTo (ax, ay) (bx, by) -> Just (MoveBy (id, bx - ax, by - ay))
       EndAt (x, y) -> Just (Release id)
       _ -> Nothing
    _ -> Nothing


isJustAction : Maybe DragAction -> Bool
isJustAction ma =
  case ma of
    Just a -> True
    Nothing -> False

fromJust : Maybe a -> a
fromJust ma =
  case ma of
    Just a -> a

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
     Signal.filter removeNonEvents (EndAt (0, 0)) 
       (Signal.foldp makeMouseEvent (EndAt (0, 0)) 
       (Signal.map2 (,) Mouse.isDown (Debug.watch "pos" <~ Mouse.position)))
