module SignalProcessing where

import Debug
import Signal exposing (..)
import Maybe exposing (..)
import Constants exposing (..)
import Dict exposing (insert)

-- mine
import Model exposing (..)
import Drag exposing (dragSignal)
import Eval exposing (evalStep)
--import View exposing (expToElsAndForms)
import SnapBlocks exposing (checkCombine)


dummyDragMailbox = mailbox Release

selectBlock = mailbox Nothing

blockTransform : Signal.Mailbox BlockAction
blockTransform = Signal.mailbox None

evalMailbox = (Debug.watch "mb" (mailbox False))


-- - - - -  R O U T I N G - A C T I O N S  - - - - -

fromBlockAction : BlockAction -> Action
fromBlockAction ba = BAction ba

fromDragAction : DragAction -> Action
fromDragAction da = DAction da

fromEvalAction : EvalAction -> Action
fromEvalAction ea = EAction ea

handleBlockSignal : Signal Action
handleBlockSignal = Signal.map fromBlockAction blockTransform.signal

handleDragSignal : Signal Action
handleDragSignal = Signal.map fromDragAction dragSignal

handleEvalSignal : Signal Action
handleEvalSignal = Signal.map fromEvalAction (Debug.watch "eval " <~ evalMailbox.signal)

allUpdateSignals : Signal Action
allUpdateSignals = Signal.merge (Signal.merge handleDragSignal handleBlockSignal) handleEvalSignal

processAnyAction : (DragAction -> Model -> Model) -> (BlockAction -> Model -> Model) -> (EvalAction -> Model -> Model) -> Action -> Model -> Model
processAnyAction funcDragAction funcBlockAction funcEvalAction action model =
    case action of
        DAction a -> funcDragAction a model
        BAction a -> funcBlockAction a model
        EAction a -> funcEvalAction a model


signalRouter :  Action -> Model -> Model
signalRouter sAction model = processAnyAction updateDrag updateBlock updateEval sAction model


-- - - - - - -  U P D A T E - M O D E L  - - - - - - - - - 

updateDrag : DragAction  -> Model -> Model
updateDrag drag m = 
        case drag of
          MoveBy (id, dx, dy) -> doDrag m id (dx, dy)
          Release id -> checkCombine m id 
          _ -> m

updateBlock : BlockAction -> Model -> Model
updateBlock action m = 
    case action of
        Add blockTemp -> {m | 
                          blocks <- insert m.nextID (blockTemp m.nextID) m.blocks
                          , nextID <- m.nextID + 1}
        _ -> m


updateEval : EvalAction -> Model -> Model
updateEval action m =
            if action then evalStep m else m


-- - - - - - -  D R A G G I N G  - - - - - - - - - 

doDrag : Model -> ID -> Model.Position -> Model
doDrag m id pos = 
    {m | blocks <- 
      Dict.insert id (dragBlock (Dict.get (Debug.watch "trying to drag" id) m.blocks) pos) m.blocks
    }


dragBlock : Maybe Block -> Model.Position -> Block
dragBlock mBlock position =
  case mBlock of
    Just block -> {block | pos <- moveBy position block.pos}
--    _ -> {id = 0, exp = E (R []), ele = [], forms = [], pos = (-1000, -1000), selected = False}


moveBy : (Int, Int) -> (Int, Int) -> (Int, Int)
moveBy (dx, dy) (x, y) = (x + dx, y - dy)


