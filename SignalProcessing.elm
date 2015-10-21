module SignalProcessing where

import Debug
import Signal exposing (..)
import Maybe exposing (..)
import Constants exposing (..)
import Dict exposing (insert)
import Window

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

evalMailbox = (Debug.watch "mb" (mailbox NoEval))

clickMailbox = mailbox 0


-- - - - -  R O U T I N G - A C T I O N S  - - - - -

fromBlockAction : BlockAction -> Action
fromBlockAction ba = BAction ba

fromDragAction : DragAction -> Action
fromDragAction da = DAction da

fromEvalAction : EvalAction -> Action
fromEvalAction ea = EAction ea

fromDimAction : (Int, Int) -> Action
fromDimAction da = WindowAction da

fromClickAction : Int -> Action
fromClickAction ca = ClickAction ca

handleBlockSignal : Signal Action
handleBlockSignal = Signal.map fromBlockAction blockTransform.signal

handleDragSignal : Signal Action
handleDragSignal = Signal.map fromDragAction dragSignal

handleEvalSignal : Signal Action
handleEvalSignal = Signal.map fromEvalAction (Debug.watch "eval " <~ evalMailbox.signal)

handleDimSignal : Signal Action
handleDimSignal = Signal.map fromDimAction Window.dimensions

handleClickSignal : Signal Action
handleClickSignal = Signal.map fromClickAction clickMailbox.signal

allUpdateSignals : Signal Action
allUpdateSignals = Signal.merge (Signal.merge (Signal.merge (Signal.merge handleDragSignal handleBlockSignal) handleEvalSignal) handleDimSignal) handleClickSignal

processAnyAction : (DragAction -> Model -> Model) -> (BlockAction -> Model -> Model) ->  (Model -> Model) -> (ClickAction -> Model -> Model) -> Action -> List Model -> List Model

processAnyAction funcDragAction funcBlockAction funcEvalAction funcClickAction action modelList =
  case modelList of
    [] -> [emptyModel]
    model::models ->
        case action of
            DAction a -> (funcDragAction a model)  :: models
            BAction a -> (funcBlockAction a model) :: model :: models
            EAction Forward -> funcEvalAction model :: model :: models
            EAction Backward -> models
            EAction NoEval -> model :: models
            ClickAction id -> (updateClick id model) :: models
            WindowAction (w, h) -> {model | dims <- (w, h)} :: models


signalRouter : Action -> List Model -> List Model
signalRouter sAction models = processAnyAction updateDrag updateBlock updateEval updateClick sAction models


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


updateEval : Model -> Model
updateEval m = evalStep m 

updateClick : ID -> Model -> Model
updateClick id m = 
  let 
      unClickedM = case Dict.get m.clicked m.blocks of
                  Just block -> {m | clicked <- 0, blocks <- Dict.insert m.clicked {block | selected <- False} m.blocks }
                  Nothing -> m
      doClick block blocks= Dict.insert block.id {block | selected <- True} blocks
  in 
      case Dict.get id m.blocks of
          Just block -> {unClickedM | blocks <- doClick block unClickedM.blocks, clicked <- id}
          Nothing -> m


-- - - - - - -  D R A G G I N G  - - - - - - - - - 

doDrag : Model -> ID -> Model.Position -> Model
doDrag m id pos = 
   case Dict.get (Debug.watch "trying to drag" id) m.blocks of
        Just block -> if didDelete (fst m.dims) block pos
                        then {m | blocks <- Dict.remove block.id m.blocks}
                        else {m | blocks <-  Dict.insert id (dragBlock block pos) m.blocks}
        _ -> m



dragBlock : Block -> Model.Position -> Block
dragBlock block position = {block | pos <- moveBy position block.pos}


moveBy : (Int, Int) -> (Int, Int) -> (Int, Int)
moveBy (dx, dy) (x, y) = (x + dx, y - dy)

didDelete : Int -> Block -> Model.Position  -> Bool
didDelete w block pos = False
--fst (moveBy pos block.pos) <  -(((Debug.watch "w is" w//2) ) ) + 30


