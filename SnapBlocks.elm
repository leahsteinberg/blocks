module SnapBlocks where


import Dict
import Debug

import Model exposing (..)
import Constants exposing (..)
import View exposing (expToElsAndForms)





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
      H hof -> collisionHOF block id model
      RE rockExp ->
          case rockExp of
              R rocks -> collisionRock block id model
              _ -> model
      _ -> model


collide :(Block -> Block -> Maybe Block) -> Block -> Block -> Maybe Block -> Maybe Block
collide collisionChecker mainBlock otherBlock maybeCollidedBlock =
  case maybeCollidedBlock of
    Just collided -> maybeCollidedBlock
    _ -> collisionChecker mainBlock otherBlock



--- TO DO TODO make this work!!!
collisionHOF : Block -> ID -> Model -> Model
collisionHOF block id model =
  let 
      otherBlocks = Dict.values model.blocks
      mCollidedBlockLeft = List.foldl (collide checkLeftCollisionsHOF block) Nothing otherBlocks
  in
      case  mCollidedBlockLeft of
        Just collidedBlock -> addHOF collidedBlock block model
        _ -> 
          let
              mCollidedBlockRight = List.foldl (collide checkRightCollisionsHOF block) Nothing otherBlocks
          in
              case mCollidedBlockRight of
                  Just collidedBlock -> addHOF block collidedBlock model
                  _ -> model


checkRightCollisionsHOF : Block -> Block -> Maybe Block
checkRightCollisionsHOF hofBlock otherBlock =
    let
        hh = hofHeight // 2
        hofRightCorners = findRightCorners hofBlock
        otherLeftCorners = ((fst otherBlock.pos, snd otherBlock.pos + hh), (fst otherBlock.pos, snd otherBlock.pos - hh))
    in
        if otherBlock.id == hofBlock.id then Nothing else
          if closeEnough hofRightCorners otherLeftCorners then Just (otherBlock) else Nothing


checkLeftCollisionsHOF : Block -> Block -> Maybe Block
checkLeftCollisionsHOF hofBlock otherBlock =
    let 
        hh = hofHeight // 2
        hofLeftCorners = (Debug.watch "left corners of right thing **" ((fst hofBlock.pos, snd hofBlock.pos + hh), (fst hofBlock.pos, snd hofBlock.pos - hh)))
        otherRightCorners = (Debug.watch "right corners of left thing**" (findRightCorners otherBlock))
    in
        if hofBlock.id == otherBlock.id then Nothing else
          if closeEnough hofLeftCorners otherRightCorners then Just otherBlock else Nothing



collisionRock : Block -> ID -> Model -> Model
collisionRock block id m =
    let 
        leftCorners =  findLeftCornersRock block
        otherBlocks = Dict.values m.blocks
        mCollidedBlock = List.foldl (collide checkCollisionOnRock block) Nothing otherBlocks

    in
        case mCollidedBlock of 
          Just collidedBlock -> addRocks collidedBlock block m
          _ -> m






checkCollisionOnRock : Block -> Block -> Maybe Block
checkCollisionOnRock rockBlock blockOnLeft =
  let 
      blockLeftsCorners = (findRightCorners blockOnLeft)
      rockBlockCorners = (findLeftCornersRock rockBlock)
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


absVal x = if x < 0 then -x else x

closeEnough : ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
closeEnough ((upX1, upY1), (downX1, downY1)) ((upX2, upY2), (downX2, downY2)) =
  let 
      xUpDistance = absVal (upX1 - upX2)
      xDownDistance = absVal (downX1 - downX2)
      yUpDistance = absVal (upY1 - upY2)
      yDownDistance = absVal (downY1 - downY2)
      combined = xUpDistance + yUpDistance + xDownDistance + yDownDistance
  in
      combined < 90

--checkLeftSide : ((Int, Int), (Int, Int)) -> Block -> (ID, Maybe Block)
--checkLeftSide leftCorners block =
--  let
--      rightCorners = findRightCorners block


findRightCorners : Block -> ((Int, Int), (Int, Int))
findRightCorners block = 
  case block.exp of
    H hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth), (snd block.pos))
    F func -> 
      let 
          fw = funcWidth // 2
          fh = funcHeight // 2
          (x, y) = (block.pos)
      in
          ((x + fw, y + fh), (x + fw, y - fh))
    RE rockExp ->
      case rockExp of
        Higher hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth), (snd block.pos)) ------ this case should never happen
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
                R rocks -> ((100000,100000), (100000,1000000))
--                  --((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight))
          Nothing ->  ((rXPos, rYPos + hofHeight//2), (rXPos, rYPos - hofHeight//2))
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
      ((x-26, y + hOffset), (x-26, y - hOffset))
      --((x - wOffset, y + hOffset), (x - wOffset, y - hOffset))



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


-- - - - -  A D D - B L O C K S - P O S T - C O L L I S I O N - - - -----

addHOF : Block -> Block -> Model -> Model
addHOF big little model =
  let 
      bigID = big.id
      newExp = addExp big.exp little.exp
      (newEls, newForms) = expToElsAndForms newExp bigID
      newBlock = {big | exp <- newExp
                        , ele <- newEls
                        , forms <- newForms }
      newDict = Dict.remove little.id (Dict.insert bigID newBlock model.blocks)
  in
      {model | blocks <- newDict}


addExp : Exp -> Exp -> Exp
addExp bigExp littleExp =
  case bigExp of
    H hof -> H (addToHof hof littleExp)
    RE rockExp ->
      case rockExp of
          R rocks -> bigExp
          Higher hofNested -> RE (Higher (addToHof hofNested littleExp))




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
        Map mFunc mRockExp -> Map mFunc (maybeRockExp mRockExp)


addToHof : HOF -> Exp -> HOF
addToHof hof littleExp =
  case hof of
    Filter mFunc mRE -> Filter mFunc (addToRockExp mRE littleExp)
    Map mFunc mRE -> Map mFunc (addToRockExp mRE littleExp)

addToRockExp : Maybe RockExpression -> Exp -> Maybe RockExpression
addToRockExp mRE exp =
  let
      addExpToNothing exp =
        case exp of
          H hof -> Just (Higher hof)
          RE rockExp ->
            case rockExp of
              R rocks -> Just (R rocks)
              Higher hof -> Just (Higher hof)
  in
      case mRE of
          Just re -> 
            case re of
              Higher hof -> Just (Higher (addToHof hof exp))
              R rocks -> Just (R rocks)
          Nothing -> addExpToNothing exp



--addHOFExp : Exp -> Exp
--addHOFExp bigExp littleExp =
--    case bigExp of
--    H hof -> 


        
addRocks : Block -> Block -> Model -> Model
addRocks bigBlock rockBlock model =
  let 
      bigID = bigBlock.id
      newExp = addRocksExp bigBlock.exp rockBlock.exp
      (newEls, newForms) = expToElsAndForms newExp bigID
      newBlock = {bigBlock | exp <- newExp
                            , ele <- newEls
                            , forms <- newForms}
  in
      {model | blocks <- Dict.remove rockBlock.id (Dict.insert bigID newBlock model.blocks)}

      


addRocksExp : Exp -> Exp -> Exp
addRocksExp bigExp rockExp =
  case bigExp of
    H hof -> H (addRocksHOF hof rockExp)
    RE oldRockExp ->
      case oldRockExp of
        Higher higherOrderFunc -> RE (Higher (addRocksHOF higherOrderFunc rockExp))


--addRocksHOF : HOF -> Exp -> HOF
--addRocksHOF bigHof rockExp =
--  let 
--      maybeRockExp mRockExp = 
--          case mRockExp of
--              Just oldRockExp -> 
--                  case oldRockExp of
--                    R rocks -> Just (R rocks)
--                    Higher hof -> Just (Higher (addRocksHOF hof rockExp))
--              Nothing -> 
--                case rockExp of
--                  RE r -> Just (r)

--  in
--      case bigHof of
--        Filter mFunc mRockExp -> Filter mFunc (maybeRockExp mRockExp)
--        Map mFunc mRockExp -> Map mFunc (maybeRockExp mRockExp)


