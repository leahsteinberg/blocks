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
      expLeftCorners = (Debug.watch "exp left" (findLeftCornersExp block))
      otherBlocks = Dict.values model.blocks
      mCollidedBlock = List.foldl (collide (checkExpCollisions expLeftCorners id)) Nothing otherBlocks
  in
      case mCollidedBlock of
        Just collidedBlock -> combineBlocks addExp collidedBlock block model
        _ -> model


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
      hofRightCorners = (Debug.watch "right corner" (getHOFRightCorners otherBlock))
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


---- TO DO TODO make this work!!!
--collisionHOF : Block -> ID -> Model -> Model
--collisionHOF block id model =
--  let 
--      otherBlocks = Dict.values model.blocks
--      mCollidedBlockLeft = List.foldl (collide checkLeftCollisionsHOF block) Nothing otherBlocks
--  in
--      case  mCollidedBlockLeft of
--        Just collidedBlock -> addHOF collidedBlock block model
--        _ -> 
--          let
--              mCollidedBlockRight = List.foldl (collide checkRightCollisionsHOF block) Nothing otherBlocks
--          in
--              case mCollidedBlockRight of
--                  Just collidedBlock -> addHOF block collidedBlock model
--                  _ -> model


--checkRightCollisionsHOF : Block -> Block -> Maybe Block
--checkRightCollisionsHOF hofBlock otherBlock =
--    let
--        hh = hofHeight // 2
--        hofRightCorners = findRightCorners hofBlock
--        otherLeftCorners = ((fst otherBlock.pos, snd otherBlock.pos + hh), (fst otherBlock.pos, snd otherBlock.pos - hh))
--    in
--        if otherBlock.id == hofBlock.id then Nothing else
--          if closeEnough hofRightCorners otherLeftCorners then Just (otherBlock) else Nothing


--checkLeftCollisionsHOF : Block -> Block -> Maybe Block
--checkLeftCollisionsHOF hofBlock otherBlock =
--    let 
--        hh = hofHeight // 2
--        hofLeftCorners = (Debug.watch "left corners of right thing **" ((fst hofBlock.pos, snd hofBlock.pos + hh), (fst hofBlock.pos, snd hofBlock.pos - hh)))
--        otherRightCorners = (Debug.watch "right corners of left thing**" (findRightCorners otherBlock))
--    in
--        if hofBlock.id == otherBlock.id then Nothing else
--          if closeEnough hofLeftCorners otherRightCorners then Just otherBlock else Nothing



--collisionRock : Block -> ID -> Model -> Model
--collisionRock block id m =
--    let 
--        leftCorners =  findLeftCornersRock block
--        otherBlocks = Dict.values m.blocks
--        mCollidedBlock = List.foldl (collide checkCollisionOnRock block) Nothing otherBlocks

--    in
--        case mCollidedBlock of 
--          Just collidedBlock -> addRocks collidedBlock block m
--          _ -> m






--checkCollisionOnRock : Block -> Block -> Maybe Block
--checkCollisionOnRock rockBlock blockOnLeft =
--  let 
--      blockLeftsCorners = (findRightCorners blockOnLeft)
--      rockBlockCorners = (findLeftCornersRock rockBlock)
--  in
--      if blockOnLeft.id == rockBlock.id then Nothing else 
--          if closeEnough blockLeftsCorners rockBlockCorners then Just blockOnLeft else Nothing



--absVal x = if x < 0 then -x else x

--closeEnough : ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Bool
--closeEnough ((upX1, upY1), (downX1, downY1)) ((upX2, upY2), (downX2, downY2)) =
--  let 
--      xUpDistance = absVal (upX1 - upX2)
--      xDownDistance = absVal (downX1 - downX2)
--      yUpDistance = absVal (upY1 - upY2)
--      yDownDistance = absVal (downY1 - downY2)
--      combined = xUpDistance + yUpDistance + xDownDistance + yDownDistance
--  in
--      combined < 90



--findRightCorners : Block -> ((Int, Int), (Int, Int))
--findRightCorners block = 
--  case block.exp of
--    H hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth), (snd block.pos))
--    F func -> 
--      let 
--          fw = funcWidth // 2
--          fh = funcHeight // 2
--          (x, y) = (block.pos)
--      in
--          ((x + fw, y + fh), (x + fw, y - fh))
--    RE rockExp ->
--      case rockExp of
--        Higher hof -> findRightCornersHOF hof ((fst block.pos) + (hofWidth), (snd block.pos))  this case should never happen
--        R rocks -> 
--          let 
--              rw = rockListWidth // 2
--              rh = rockHeight // 2
--              x = fst block.pos
--              y = snd block.pos
--          in
--              ((x + rw, y + rh), (x + rw, y - rh))



--findRightCornersPos : Exp -> (Int, Int) -> ((Int, Int), (Int, Int))
--findRightCornersPos exp (rXPos, rYPos) =
--  case exp of
--    H hof -> findRightCornersHOF hof (rXPos, rYPos) cant be a first one
--    RE rockExp ->
--      case rockExp of
--        Higher hof -> findRightCornersHOF hof (rXPos +(hofWidth), rYPos)
--        R rocks -> ((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight))



--findRightCornersHOF : HOF -> (Int, Int) -> ((Int, Int), (Int, Int))
--findRightCornersHOF hof (rXPos, rYPos) =
--  let 
--      addX mRockExp =
--        case mRockExp of
--          Just rockExp ->
--            case rockExp of 
--                Higher hof -> findRightCornersHOF hof ((rXPos + hofWidth), rYPos)
--                R rocks -> ((100000,100000), (100000,1000000))
--                  ((rXPos + rockListWidth, rYPos + rockHeight), (rXPos + rockListWidth, rYPos - rockHeight))
--          Nothing ->  ((rXPos, rYPos + hofHeight//2), (rXPos, rYPos - hofHeight//2))
--  in
--      case hof of
--        Filter mFunc mRockExp -> addX mRockExp
--        Map mFunc mRockExp -> addX mRockExp


--findLeftCornersRock : Block -> ((Int, Int), (Int, Int))
--findLeftCornersRock block  =
--  let 
--      (x, y) = block.pos
--      wOffset = rockListWidth//2
--      hOffset = rockHeight//2
--  in
--      ((x-26, y + hOffset), (x-26, y - hOffset))
--      ((x - wOffset, y + hOffset), (x - wOffset, y - hOffset))






-- - - - -  A D D - B L O C K S - P O S T - C O L L I S I O N - - - -

--addHOF : Block -> Block -> Model -> Model
--addHOF big little model =
--  let 
--      bigID = big.id
--      newExp = addExp big.exp little.exp
--      (newEls, newForms) = expToElsAndForms newExp bigID
--      newBlock = {big | exp <- newExp
--                        , ele <- newEls
--                        , forms <- newForms }
--      newDict = Dict.remove little.id (Dict.insert bigID newBlock model.blocks)
--  in
--      {model | blocks <- newDict}


--addExp : Exp -> Exp -> Exp
--addExp bigExp littleExp =
--  case bigExp of
--    H hof -> H (addToHof hof littleExp)
--    RE rockExp ->
--      case rockExp of
--          R rocks -> bigExp
--          Higher hofNested -> RE (Higher (addToHof hofNested littleExp))




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


--addToHof : HOF -> Exp -> HOF
--addToHof hof littleExp =
--  case hof of
--    Filter mFunc mRE -> Filter mFunc (addToRockExp mRE littleExp)
--    Map mFunc mRE -> Map mFunc (addToRockExp mRE littleExp)

--addToRockExp : Maybe RockExpression -> Exp -> Maybe RockExpression
--addToRockExp mRE exp =
--  let
--      addExpToNothing exp =
--        case exp of
--          H hof -> Just (Higher hof)
--          RE rockExp ->
--            case rockExp of
--              R rocks -> Just (R rocks)
--              Higher hof -> Just (Higher hof)
--  in
--      case mRE of
--          Just re -> 
--            case re of
--              Higher hof -> Just (Higher (addToHof hof exp))
--              R rocks -> Just (R rocks)
--          Nothing -> addExpToNothing exp



        
--addRocks : Block -> Block -> Model -> Model
--addRocks bigBlock rockBlock model =
--  let 
--      bigID = bigBlock.id
--      newExp = addRocksExp bigBlock.exp rockBlock.exp
--      (newEls, newForms) = expToElsAndForms newExp bigID
--      newBlock = {bigBlock | exp <- newExp
--                            , ele <- newEls
--                            , forms <- newForms}
--  in
--      {model | blocks <- Dict.remove rockBlock.id (Dict.insert bigID newBlock model.blocks)}

      


--addRocksExp : Exp -> Exp -> Exp
--addRocksExp bigExp rockExp =
--  case bigExp of
--    H hof -> H (addRocksHOF hof rockExp)
--    RE oldRockExp ->
--      case oldRockExp of
--        Higher higherOrderFunc -> RE (Higher (addRocksHOF higherOrderFunc rockExp))



