module SignalProcessing where

import Debug
import Signal exposing (..)
import Maybe exposing (..)
import Constants exposing (..)
import Dict exposing (insert)

-- mine
import Model exposing (..)
import Drag exposing (dragSignal)


dummyDragMailbox = mailbox Release

selectBlock = mailbox Nothing

blockTransform : Signal.Mailbox BlockAction
blockTransform = Signal.mailbox None


-- - - - -  R O U T I N G - A C T I O N S  - - - - -

fromBlockAction : BlockAction -> Action
fromBlockAction ba = BAction ba

fromDragAction : DragAction -> Action
fromDragAction da = DAction da

handleBlockSignal : Signal Action
handleBlockSignal = 
            Signal.map fromBlockAction (Debug.watch "block transform" <~ blockTransform.signal)

handleDragSignal : Signal Action
handleDragSignal = 
            Signal.map fromDragAction (Debug.watch "drag signal" <~ dragSignal)

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

updateDrag : DragAction  -> Model -> Model
updateDrag drag m = 
        case drag of
          MoveBy (id, dx, dy) -> doDrag m id (dx, dy)
          Release (id, x, y) -> checkCombine m id (x, y)
          _ -> m

updateBlock : BlockAction -> Model -> Model
updateBlock action m = 
    case action of
        Add blockTemp -> {m | 
            blocks <- insert m.nextID (blockTemp m.nextID) m.blocks
            , nextID <- m.nextID + 1}
        _ -> m


-- - - - - - - - -  C O M B I N E - B L O C K  - - - - - - - - -

checkCombine : Model -> ID -> Model.Position -> Model
checkCombine m id p = m








-- - - - - - - - -  D R A G G I N G  - - - - - - - - - - 



doDrag : Model -> ID -> Model.Position -> Model
doDrag m id pos = 
    {m | blocks <- 
      Dict.insert id (dragBlock (Dict.get id m.blocks) pos) m.blocks
    }


dragBlock : Maybe Block -> Model.Position -> Block
dragBlock mBlock position =
  case mBlock of
    Just block -> {block | pos <- moveBy position block.pos}

--moveBy : (Int, Int) -> (Int, Int) -> (Int, Int)
--moveForest : ID -> Model.Position -> List Exp -> List Exp -> List Exp
--moveForest id pos forest1 forest2 =
--  case forest1 of
--    [] -> forest2
--    (x::xs) ->
--        let (changed, newExp) = moveExp x id pos
--        in 
--            if changed then newExp :: (xs ++ forest2) else moveForest id pos xs (newExp :: forest2)

    


--moveExp : Exp -> ID -> Model.Position -> (Bool, Exp)
--moveExp exp id pos = 
--    case exp of
--        H hof -> moveHOF hof id pos
----        F func -> moveFunc func id
----        R rocks -> moveRocks rocks

--moveHOF : HOF -> ID -> Model.Position -> (Bool, Exp)
--moveHOF hof id pos =
--  case hof of
--    Filter block mF mR -> 
--      let (changed, newBlock) = moveBlock block id pos
--      in 
--          (changed, H (Filter newBlock mF mR))

--    Map block mF mR -> 
--      let (changed, newBlock) = moveBlock block id pos
--      in
--          (changed, H (Map newBlock mF mR))


--moveBlock : Block -> ID -> Model.Position -> (Bool, Block)
--moveBlock block id pos =
--    if block.id == id then (True, {block | pos <- moveBy pos block.pos}) else (False, block)

moveBy : (Int, Int) -> (Int, Int) -> (Int, Int)
moveBy (dx, dy) (x, y) = (x + dx, y - dy)


