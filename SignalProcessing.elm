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
            Signal.map fromBlockAction blockTransform.signal

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

-- ------- DE BUGGING -----

--cornersSignal : Signal DragAction -> Signal (Maybe CornerAction)
--cornersSignal sDrag =
--    Signal.map (checkSignal 

--checkSignal Model -> DragAction -> Maybe CornerAction
--checkSignal da =
--  case da of
--    Release id -> findLeftCornersId id
--    _ -> Nothing


--findLeftCornersId : ID -> ((Int, Int), (Int, Int))
--findLeftCornersId id =
--  case Dict.get id 

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
collisionDetection block id model = 
  case block.exp of
      RE rockExp ->
          case rockExp of
              R rocks -> collisionRock block id model
              _ -> model
      _ -> model

collisionRock : Block -> ID -> Model -> Model
collisionRock block id m =
    let 
        leftCorners =  findLeftCornersRock block
        otherBlocks = Dict.values m.blocks
        mCollidedBlock = List.foldl (collide block) Nothing otherBlocks

    in
        case mCollidedBlock of 
          Just collidedBlock -> addRocks collidedBlock block m
          _ -> m

        
addRocks : Block -> Block -> Model -> Model
addRocks bigBlock rockBlock model =
  let 
      bigID = bigBlock.id
      newBlock = {bigBlock | exp <- addRocksExp bigBlock.exp rockBlock.exp}
  in
      {model | blocks <- Dict.insert bigID newBlock model.blocks}

      


addRocksExp : Exp -> Exp -> Exp
addRocksExp bigExp rockExp =
  case bigExp of
    H hof -> H (addRocksHOF hof rockExp )
    RE oldRockExp ->
      case oldRockExp of
        Higher higherOrderFunc -> RE (Higher (addRocksHOF higherOrderFunc rockExp))


addRocksHOF : HOF -> Exp -> HOF
addRocksHOF bigHof rockExp =
  let 
      maybeRockExp mRockExp = 
          case mRockExp of
              Just oldRockExp -> 
                  case oldRockExp of
                    R rocks -> Just (R rocks)
                    Higher hof -> Just (Higher (addRocksHOF hof rockExp))
              Nothing -> 
                case rockExp of
                  RE r -> Just (r)

  in
      case bigHof of
        Filter mFunc mRockExp -> Filter mFunc (maybeRockExp mRockExp)
        Map mFunc mRockExp -> Filter mFunc (maybeRockExp mRockExp)


collide : Block -> Block -> Maybe Block -> Maybe Block
collide mainBlock otherBlock maybeCollidedBlock =
  case maybeCollidedBlock of
    Just collided -> maybeCollidedBlock
    _ -> checkCollisionOnRock otherBlock mainBlock


checkCollisionOnRock : Block -> Block -> Maybe Block
checkCollisionOnRock blockOnLeft rockBlock =
  let 
      blockLeftsCorners = (Debug.watch "left corners" (findRightCorners blockOnLeft))
      rockBlockCorners = (Debug.watch "right corners (rocks)" (findLeftCornersRock rockBlock))
  in
      if blockOnLeft.id == rockBlock.id then Nothing else 
          if closeEnough blockLeftsCorners rockBlockCorners then Just blockOnLeft else Nothing



--checkEachSide : Block -> ID -> Model -> Model 
--checkEachSide block id m =
--  let 
--      rightCorners = findRightCorners block
--  in
--      if isRocks block then checkRightSide id rightCorners else 
--          if not checkRightSide id rightCorners then checkLeftSide id (findLeftCorners block)



--checkLeftSide : Block -> List Block -> Model -> Model
--checkLeftSide block [] m = m
--checkLeftSide block (x::xs) m =



--checkLeftSide : Block -> Block -> Model -> (ID, Maybe Block)
--checkLeftSide dragBlock otherBlock =
--  let 
--      leftCorners = findLeftCorners (startsWithHOF dragBlock) dragBlock
--  in
--      List.foldr (\b, answer ->


--        ) (Dict.values m.blocks)


closeEnough : ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
closeEnough ((upX1, upY1), (downX1, downY1)) ((upX2, upY2), (downX2, downY)) = True
--checkLeftSide : ((Int, Int), (Int, Int)) -> Block -> (ID, Maybe Block)
--checkLeftSide leftCorners block =
--  let
--      rightCorners = findRightCorners block


findRightCorners : Block -> ((Int, Int), (Int, Int))
findRightCorners block =
  case block.exp of
    H hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth//2), (snd block.pos))
    F func -> 
      let 
          fw = funcWidth // 2
          fh = funcHeight // 2
          (x, y) = (block.pos)
      in
          ((x + fw, y + fh), (x + fw, y - fh))
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
              ((x + rw, y + rh), (x + rw, y - rh))


findRightCornersPos : Exp -> (Int, Int) -> ((Int, Int), (Int, Int))
findRightCornersPos exp (rXPos, rYPos) =
  case exp of
    H hof -> findRightCornersHOF hof (rXPos, rYPos)-- cant be a first one
    RE rockExp ->
      case rockExp of
        Higher hof -> findRightCornersHOF hof (rXPos +(hofWidth), rYPos)
        R rocks -> ((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight))



findRightCornersHOF : HOF -> (Int, Int) -> ((Int, Int), (Int, Int))
findRightCornersHOF hof (rXPos, rYPos) =
  let 
      addX mRockExp =
        case mRockExp of
          Just rockExp ->
            case rockExp of 
                Higher hof -> findRightCornersHOF hof ((rXPos + hofWidth), rYPos)
                R rocks -> ((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight))
          _ -> ((rXPos, rYPos + hofHeight), (rXPos, rYPos - hofHeight))
  in
      case hof of
        Filter mFunc mRockExp -> addX mRockExp
        Map mFunc mRockExp -> addX mRockExp


findLeftCornersRock : Block -> ((Int, Int), (Int, Int))
findLeftCornersRock block  =
  let 
      (x, y) = block.pos
      wOffset = rockListWidth//2
      hOffset = rockHeight//2
  in
      ((x - wOffset, y + hOffset), (x - wOffset, y - hOffset))



--startsWithHOF : Block -> Bool
--startsWithHOF block =
--  case block.exp of
--    H hof -> True
--    _ -> False

--isRocks : Block -> Bool
--isRocks block =
--  case block.exp of
--    RE r -> case r of
--            R rocks -> True
--            _ -> False
--    _ -> False


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


