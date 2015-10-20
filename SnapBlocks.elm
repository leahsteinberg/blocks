module SnapBlocks where

 
import Dict
import Debug
import Model exposing (..)
import Constants exposing (..)
import ViewFragment exposing (fragmentToForms)



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
    E exp -> collisionExp block id model
    H hof -> collisionHOF block id model
    F func -> collisionFunc block func id model


collisionHOF : Block -> ID -> Model -> Model
collisionHOF block id model =
  let
      hofRightCorners = getHOFRightCorners block
      otherBlocks = Dict.values model.blocks
      mCollidedBlock = List.foldl (collide (checkHOFCollisions hofRightCorners id)) Nothing otherBlocks
  in
      case mCollidedBlock of
        Just collidedBlock -> combineBlocks addExp block collidedBlock model
        _ -> model

collisionExp : Block -> ID -> Model -> Model
collisionExp block id model =
  let
      expLeftCorners = (findLeftCornersExp block)
      otherBlocks = Dict.values model.blocks
      mCollidedBlock = List.foldl (collide (checkExpCollisions expLeftCorners id)) Nothing otherBlocks
  in
      case mCollidedBlock of
        Just collidedBlock -> combineBlocks addExp collidedBlock block model
        _ -> model

collisionFunc : Block -> Func -> ID -> Model -> Model
collisionFunc block func id model =
  let
      funcLeftCorners = (Debug.watch "left func" (findLeftCornersFunc block))
      otherBlocks = Dict.values model.blocks
      mCollidedBlock = List.foldl (collide (checkFuncCollisions funcLeftCorners func id)) Nothing otherBlocks
  in
      case mCollidedBlock of
          Just collidedBlock -> combineBlocks (\f1 f2 -> f1) collidedBlock block model
          _ -> model


findLeftCornersFunc : Block -> ((Int, Int), (Int, Int))
findLeftCornersFunc block = 
  let
      (x, y) = block.pos
      hw = funcWidth // 2
  in
      ((x + hw + funcWidth - 3, y + funcWidth), (x + hw + funcWidth - 3, y - funcWidth))


checkFuncCollisions : ((Int, Int), (Int, Int)) -> Func -> ID -> Block -> Maybe Block 
checkFuncCollisions funcLeftCorners func id otherBlock =
        case otherBlock.exp of
            E exp -> 
              let
                  mExp = checkFuncCollisionsExp funcLeftCorners 0 func exp otherBlock
              in
                  case mExp of
                    Just returnExp -> Just ({otherBlock | exp <- (E returnExp)})
                    _ -> Nothing
            H hof -> 
              let mHOF = checkFuncCollisionsHof funcLeftCorners func hof otherBlock
              in
                  case mHOF of
                    Just returnHOF -> Just ({otherBlock | exp <- (H returnHOF)})
                    _ -> Nothing
            _ -> Nothing




checkFuncCollisionsHof : ((Int, Int), (Int, Int)) -> Func -> HOF -> Block -> Maybe HOF
checkFuncCollisionsHof funcLeftCorners func hof otherBlock = 
  let
      possiblyAdd mExistingFunc newFunc = 
        case mExistingFunc of 
          Just func -> Just func
          Nothing -> Just newFunc
      addToHOF = 
              case hof of 
        Filter mFunc mRocks -> Just (Filter (possiblyAdd mFunc func) mRocks)
        Map mFunc mRocks -> Just (Map (possiblyAdd mFunc func) mRocks)
      (x, y) = otherBlock.pos
      hofMiddlePoints = ((x + hofWidth//2, y + hofHeight), (x + hofWidth//2, y - hofHeight))

  in
      if closeEnough (Debug.watch "just hof right" hofMiddlePoints) funcLeftCorners then addToHOF else Nothing


addFuncHof : HOF -> Func -> HOF 
addFuncHof hof func = 
  let
      possiblyAdd mExistingFunc newFunc = 
        case mExistingFunc of 
          Just func -> Just func
          Nothing -> Just newFunc
  in

      case hof of 
        Filter mFunc mRocks -> Filter (possiblyAdd mFunc func) mRocks
        Map mFunc mRocks -> Map (possiblyAdd mFunc func) mRocks


checkFuncCollisionsExp : ((Int, Int), (Int, Int)) -> Int -> Func -> Exp -> Block -> Maybe Exp
checkFuncCollisionsExp funcLeftCorners xShift func otherExp block =
  case otherExp of
    C hof littleExp -> if closeEnoughFunc funcLeftCorners xShift func hof block
                            then Just (C (addFuncHof hof func) littleExp)  else 
                              let
                                  mExp =  (checkFuncCollisionsExp funcLeftCorners (xShift + 1) func littleExp block)
                              in
                                  case mExp of
                                    Just recurExp -> Just (C hof recurExp)
                                    Nothing -> Nothing
    _ -> Nothing


closeEnoughFunc : ((Int, Int), (Int, Int)) -> Int -> Func -> HOF -> Block -> Bool
closeEnoughFunc funcLeftCorners xShift func hof block = 
  let
      (x,y) = block.pos
      hofX =  (xShift* (hofWidth + blockOffset)) + (hofWidth//2)
      hofMiddlePoints = ((x + hofX, y + hofHeight), (x + hofX, y - hofHeight))
  in
      closeEnough funcLeftCorners (Debug.watch "hof middle" hofMiddlePoints)



collide : (Block -> Maybe Block) -> Block -> Maybe Block -> Maybe Block
collide collisionChecker otherBlock maybeCollidedBlock =
  case maybeCollidedBlock of
    Just collided -> maybeCollidedBlock
    _ -> collisionChecker otherBlock



checkHOFCollisions : ((Int, Int), (Int, Int)) -> ID -> Block -> Maybe Block
checkHOFCollisions hofRightCorners id otherBlock =
  case otherBlock.exp of
    E exp -> if closeEnoughOnRight hofRightCorners otherBlock then Just otherBlock else Nothing
    _ -> Nothing




checkExpCollisions : ((Int, Int), (Int, Int)) -> ID -> Block -> Maybe Block
checkExpCollisions leftCorners id otherBlock =
  case otherBlock.exp of
    H hof -> if closeEnoughOnLeft leftCorners otherBlock then Just otherBlock else Nothing
    _ -> Nothing


closeEnough : ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
closeEnough ((upX1, upY1), (downX1, downY1)) ((upX2, upY2), (downX2, downY2)) =
  let 
      xUpDistance = abs (upX1 - upX2)
      xDownDistance = abs (downX1 - downX2)
      yUpDistance = abs (upY1 - upY2)
      yDownDistance = abs (downY1 - downY2)
      combined = xUpDistance + yUpDistance + xDownDistance + yDownDistance
  in
      combined < 90

closeEnoughOnRight : ((Int, Int), (Int, Int)) -> Block -> Bool
closeEnoughOnRight rightCorners otherBlock =
  let
      expLeftCorners = findLeftCornersExp otherBlock
  in
      closeEnough expLeftCorners rightCorners


closeEnoughOnLeft : ((Int, Int), (Int, Int)) -> Block -> Bool
closeEnoughOnLeft leftCorners otherBlock =
  let
      hofRightCorners = (getHOFRightCorners otherBlock)
  in 
      closeEnough leftCorners hofRightCorners


getHOFRightCorners : Block -> ((Int, Int), (Int, Int))
getHOFRightCorners block =
  let
      (x, y) = block.pos
      halfHeight = hofHeight//2
      halfWidth = hofWidth//2
  in
      ((x + hofWidth, y + halfHeight), (x + hofWidth, y - halfHeight))


findLeftCornersExp : Block -> ((Int, Int), (Int, Int))
findLeftCornersExp block =
  case block.exp of
    E exp -> 
        case exp of
            C hof exp -> findLeftCornersComposedExp block
            R rocks -> findLeftCornersRock block


findLeftCornersRock : Block -> ((Int, Int), (Int, Int))
findLeftCornersRock block = 
  let 
      (x, y) = block.pos
      halfWidth = rockListWidth // 2 //2
      halfHeight = hofHeight // 2
  in
      ((x - 25 , y + halfHeight), (x - 25, y - halfHeight))


findLeftCornersComposedExp : Block -> ((Int, Int), (Int, Int))
findLeftCornersComposedExp block =
  let 
      (x, y) = block.pos
      halfWidth = hofWidth // 2
      halfHeight = hofHeight // 2
  in
      ((x , y + halfHeight), (x , y - halfHeight))


combineBlocks : (Fragment -> Fragment -> Fragment) -> Block -> Block -> Model -> Model
combineBlocks combiner big little model = 
  let
      bigID = big.id
      newFragment = combiner big.exp little.exp
      (newEls, newForms) = fragmentToForms newFragment bigID
      newBlock = {big | exp <- newFragment
                        , ele <- newEls
                        , forms <- newForms }
      newDict = Dict.remove little.id (Dict.insert bigID newBlock model.blocks)
  in
      {model | blocks <- newDict}

addExp : Fragment -> Fragment -> Fragment
addExp bigFragmentHOF littleFragmentExp =
  let
      bigHOF = case bigFragmentHOF of
                  H hof -> hof

      littleExp = case littleFragmentExp of
                              E exp -> exp
  in
      E (C bigHOF littleExp)