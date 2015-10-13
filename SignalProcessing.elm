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
          Release id -> checkCombine m id 
          _ -> m

updateBlock : BlockAction -> Model -> Model
updateBlock action m = 
    case action of
        Add blockTemp -> {m | 
            blocks <- insert m.nextID (blockTemp m.nextID) m.blocks
            , nextID <- m.nextID + 1}
        _ -> m


-- - - - - - - - -  C O M B I N E - B L O C K  - - - - - - - - -

checkCombine : Model -> ID -> Model
checkCombine m id = 
  let
      mBlock = Dict.get id m.blocks
  in 
      case mBlock of 
        Just block -> collisionDetection block id m
        Nothing -> m

collisionDetection : Block -> ID -> Model -> Model
collisionDetection block id model = model


--checkLeftSide : Block -> Block -> Model -> (ID, Maybe Block)
--checkLeftSide dragBlock otherBlock =
--  let 
--      leftCorners = findLeftCorners (startsWithHOF dragBlock) dragBlock
--  in
--      List.foldr (\b, answer ->


--        ) (Dict.values m.blocks)


--closeEnough : ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
--closeEnough ((upX1, upY1), (downX1, downY1)) ((upX2, upY2), (downX2, downY))

--checkLeftSide : ((Int, Int) (Int, Int)) -> Block -> (ID, Maybe Block)
--checkLeftSide leftCorners block =
--  let
--      rightCorners = findRightCorners block


findRightCorners : Block -> (((Int, Int), (Int, Int)), Exp)
findRightCorners block =
  case block.exp of
    H hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth//2), (snd block.pos))
    F func -> 
      let 
          fw = funcWidth // 2
          fh = funcHeight // 2
          (x, y) = (block.pos)
      in
          (((x + fw, y + fh), (x + fw, y - fh)), F func)
    RE rockExp ->
      case rockExp of
        Higher hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth//2), (snd block.pos)) ------ this case should never happen
        R rocks -> 
          let 
              rw = rockListWidth // 2
              rh = rockHeight // 2
              x = fst block.pos
              y = snd block.pos
          in
              (((x + rw, y + rh), (x + rw, y - rh)), RE(R rocks))


findRightCornersPos : Exp -> (Int, Int) -> (((Int, Int), (Int, Int)), Exp)
findRightCornersPos exp (rXPos, rYPos) =
  case exp of
    H hof -> findRightCornersHOF hof (rXPos, rYPos)-- cant be a first one
    RE rockExp ->
      case rockExp of
        Higher hof -> findRightCornersHOF hof ((rXPos +(hofWidth), rYPos))
        R rocks -> (((rXPos + rockListWidth, rYPos + rockHeight),(rXPos + rockListWidth, rYPos - rockHeight)), RE (R rocks))



findRightCornersHOF : HOF -> (Int, Int) -> (((Int, Int), (Int, Int)), Exp)
findRightCornersHOF hof (rXPos, rYPos) =
  let 
      addX mRockExp =
        case mRockExp of
          Just rockExp ->
            case rockExp of 
                Higher hof -> findRightCornersHOF hof ((rXPos + hofWidth), rYPos)
                R rocks -> (((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight)), RE(R rocks))
          _ -> (((rXPos, rYPos + hofHeight), (rXPos, rYPos - hofHeight)), H hof)
  in
      case hof of
        Filter mFunc mRockExp -> addX mRockExp
        Map mFunc mRockExp -> addX mRockExp


findLeftCorners : Block -> Bool -> ((Int, Int), (Int, Int))
findLeftCorners block startsHOF =
  let 
      (x, y) = block.pos
      wOffset = if startsHOF then hofWidth//2 else rockListWidth//2
      hOffset = if startsHOF then hofHeight//2 else rockHeight//2
  in
      ((x - wOffset, y + hOffset), (x - wOffset, y - hOffset))



startsWithHOF : Block -> Bool
startsWithHOF block =
  case block.exp of
    H hof -> True
    _ -> False


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


