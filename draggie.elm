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


hover = Signal.mailbox False

box = Graphics.Input.hoverable (Signal.message hover.address) (putInBox (leftAligned (fromString "LOL")))

putInBox e =
  let (sx, sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]

moveBy (dx, dy) (x, y) = (x + toFloat dx, y - toFloat dy)

type Action = Lift | MoveBy (Int,Int) | Release


drag : Bool -> MouseEvent -> Maybe (Action)
drag hovering mouseEvent =
  if hovering then
     case mouseEvent of
       MoveFromTo (ax, ay) (bx, by) -> Just (MoveBy (bx - ax, by - ay))
       _ -> Nothing
  else
  Nothing

dragSignal : Signal (Maybe Action)
dragSignal =
  Signal.map2 drag hover.signal mouseEvents

main = 
  let update m =
    case m of 
      Just (MoveBy (dx, dy)) -> moveBy (dx, dy)
      _ -> identity
  in 
     Signal.map (\p -> collage 700 700  [Graphics.Collage.move p (toForm box)])
     (foldp update (0, 0) dragSignal)




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


--main = show "hiiiii"
