module SignalProcessing where

import Debug
import Signal exposing (..)
import Maybe exposing (..)
import Constants exposing (..)

-- mine
import Model exposing (..)


dummyDragMailbox = mailbox Release

selectBlock = mailbox Nothing

hover = mailbox Nothing

blockTransform : Signal.Mailbox BlockAction
blockTransform = Signal.mailbox None


-- - - - -  R O U T I N G - A C T I O N S  - - - - -

fromBlockAction : BlockAction -> Action
fromBlockAction ba = BAction ba

fromDragAction : DragAction -> Action
fromDragAction da = DAction da

handleBlockSignal : Signal Action
handleBlockSignal = 
            Signal.map fromBlockAction (Debug.watch "block transform signal" <~ blockTransform.signal)

handleDragSignal : Signal Action
handleDragSignal = 
            Signal.map fromDragAction dummyDragMailbox.signal

allUpdateSignals : Signal Action
allUpdateSignals = Signal.merge handleDragSignal handleBlockSignal

processAnyAction : (DragAction -> Model -> Model) -> (BlockAction -> Model -> Model) -> Action -> Model -> Model
processAnyAction funcDragAction funcBlockAction action model =
    case action of
        DAction a -> funcDragAction a model
        BAction a -> funcBlockAction a model


signalRouter :  Action -> Model -> Model
signalRouter sAction model = processAnyAction updateDrag updateBlock sAction model


-- - - - - - -  U P D A T E - M O D E L  - - - - - - - - - 

updateDrag : DragAction -> Model -> Model
updateDrag action model = model

updateBlock : BlockAction -> Model -> Model
updateBlock action model =
    case action of
        Add exp -> {model | blocks <- exp :: model.blocks}
        _ -> model


-- - - - - - - - -  A D D - B L O C K  - - - - - - - - -









-- - - - - - - - -  D R A G G I N G  - - - - - - - - - - 


--drag : Maybe (Int) -> MouseEvent -> Maybe (Action)
--drag hovering mouseEvent =
--  case hovering of
--    Just id ->
--      case mouseEvent of
--       MoveFromTo (ax, ay) (bx, by) -> Just (MoveBy (id, bx - ax, by - ay))
--       _ -> Nothing
--    _ -> Nothing


--isJustAction : Maybe(Action) -> Bool
--isJustAction ma =
--  case ma of
--    Just a -> True
--    Nothing -> False

--fromJust : Maybe a -> a
--fromJust ma =
--  case ma of
--    Just a -> a

--dragSignal : DragSignal
--dragSignal =
--  let 
--      maybeDrag = (Signal.map2 drag hover.signal mouseEvents)
--      justDrags = Signal.filter isJustAction (Just Lift) maybeDrag 
--      drags = Signal.map fromJust justDrags
--  in
--      drags



--removeNonEvents : MouseEvent -> Bool
--removeNonEvents e =
--  case e of
--    NoEvent -> False
--    _ -> True

--mouseEvents : Signal MouseEvent
--mouseEvents =
--  let makeMouseEvent (down, (px, py)) oldEvent =
--    case oldEvent of
--      StartAt (ix, iy) ->
--        if down
--        then MoveFromTo (ix, iy) (px, py)
--        else EndAt (px, py)

--      MoveFromTo (ax, ay) (bx, by) ->
--        if down
--        then MoveFromTo (bx, by) (px, py)
--        else EndAt (px, py)

--      EndAt (ix, iy) ->
--        if down 
--        then StartAt (px, py)
--        else NoEvent

--      NoEvent ->
--        if down
--        then StartAt (px, py)
--        else NoEvent

--  in
--     filter removeNonEvents (EndAt (0, 0)) 
--       (foldp makeMouseEvent (EndAt (0, 0)) 
--       (Signal.map2 (,) Mouse.isDown Mouse.position))



--moveBox : Model -> ID -> (Int, Int) -> Model
--moveBox m id (dx, dy) =  {m | boxes <-
--                                  (List.foldr
--                                  (\b boxes ->
--                                    if b.id == id
--                                    then {b | pos <- moveBy (dx, dy) b.pos} :: boxes
--                                    else b :: boxes)
--                                                  []
--                                                  m.boxes)}

--updateDrag : Maybe Action  -> Model -> Model
--updateDrag drag m =
--        case drag of
--          Just (MoveBy (id, dx, dy)) -> moveBox m id (dx, dy)
--          _ -> m

