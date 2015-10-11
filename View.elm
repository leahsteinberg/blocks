module View where 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (leftAligned, centered, color, size, Element)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white, red)

-- my modules
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing(..)
import Drag exposing (makeHoverable)





-- - - - - - - - -  N E W  -  S T U F F - - - - - - - - 

expToElements : Exp -> ID -> Element
expToElements exp id =
  case exp of
    H hof -> hofToElements hof id
    RE rockExp -> 
      case rockExp of
        R rocks -> viewRocks rocks id
--    F func -> funcToElements func
--    R rockExp -> rocksToElements rockExp

hofToElements : HOF -> ID -> Element
hofToElements hof id =
  case hof of
    Filter mFunc mRockExp -> hofElement id "filter" bRed
    Map mFunc mRockExp -> hofElement id "map" bBlue



hofElement : ID -> String -> Color -> Element
hofElement id str col =
  (leftAligned (applyStyle (fromString str)))
                      |> color col
                      |> size hofWidth hofHeight
                      |> makeHoverable id










-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


fw = hofWidth/2
fh = hofHeight/2
convexEnd col = polygon [(-fw + 1, fh)
                            ,(-fw-5+1 , fh)
                            ,(-fw-5+1, fh/2)
                            ,(-fw-10+1, fh/2)
                            ,(-fw-10+1, -(fh/2))
                            ,(-fw-5+1, -(fh/2))
                            ,(-fw-5+1, -fh)
                            ,(-fw+1, -fh)
                            ,(-fw+1, fh) ]
                                  |> filled col

concaveEnd col = polygon [(fw-1, fh)
                      , (fw + 9, fh)
                      , (fw + 9, fh/2)
                      , (fw + 4,  fh/2)
                      , (fw + 4, -(fh/2))
                      , ( fw + 9, -(fh/2))
                      , (fw+9, -fh)
                      , (fw - 1, -fh)]
                      |> filled col

-- - - - - - - - -  A  P  I  - - - - - - - - - - - - - - - 

endForms col = [convexEnd col, concaveEnd col]




-- - - - - - - - -  T R A V E R S A L S  - - - - - - - - - - 
--flattenForest : List Exp -> (List Element, List Form)
--flattenForest exps = List.concatMap traverseExp exps


--traverseExp : Exp -> (List Element, List Form)
--traverseExp exp =
--  case exp of
--    H hof -> traverseHOF hof
----    F func ->
----    R rocks ->


-- - - - - - - - -  Higher Order Functions  - - - - - - - - - - 

--traverseHOF : HOF -> (List Element, List Form)
--traverseHOF hof =
--  let
--      makeHOFView block mFunc mRocks = 
--        let moveIt = move (floatPos block.pos)
--        in
--            List.map moveIt
--            ((viewHOF block)
--              ++  ((maybeFuncForm mFunc block.pos)
--              ++ (maybeRocksForm mRocks block.pos)))

--      maybeRocksForm maybeRocks parentPos =
--      case maybeRocks of
--        Just rocks -> viewRocks rocks (Just parentPos)
--        _ -> emptyRockForm parentPos

--      maybeFuncForm maybeFunc parentPos =
--      case maybeFunc of
--        Just func -> viewFunc func (Just parentPos)
--        _ -> emptyFuncForm parentPos
--  in
--      case hof of
--        Filter mPred mRocks -> List.map (move (floatPos block.pos)) (viewHOF block)
----          makeHOFView block mPred mRocks

--        Map mTransform mRocks -> List.map (move (floatPos block.pos)) (viewHOF block)--makeHOFView block mTransform mRocks


--formHOF : Int -> String -> Color -> Model.Position -> Element
--formHOF id str col pos =
--  let
--      background = (leftAligned (applyStyle (fromString str)))
--                      |> color col
--                      |> size hofWidth hofHeight
--      (x, y) = floatPos pos
--       in
--           background
--            |> makeHoverable id
--------|> move (floatPos pos)

--viewHOF : Block -> (List Element, List Form)
--viewHOF block =
--    let 
--        fw = hofWidth/2
--        fh = hofHeight/2
--        convexEnd = polygon [(-fw + 1, fh)
--                            ,(-fw-5+1 , fh)
--                            ,(-fw-5+1, fh/2)
--                            ,(-fw-10+1, fh/2)
--                            ,(-fw-10+1, -(fh/2))
--                            ,(-fw-5+1, -(fh/2))
--                            ,(-fw-5+1, -fh)
--                            ,(-fw+1, -fh)
--                            ,(-fw+1, fh) ]
--                                  |> filled bRed
--                                  |> move (floatPos block.pos)
--        concaveEnd = polygon [(fw-1, fh)
--                      , (fw + 9, fh)
--                      , (fw + 9, fh/2)
--                      , (fw + 4,  fh/2)
--                      , (fw + 4, -(fh/2))
--                      , ( fw + 9, -(fh/2))
--                      , (fw+9, -fh)
--                      , (fw - 1, -fh)]
--                      |> filled bRed
--                      |> move (floatPos block.pos)
--    in
--        ([block.form], [convexEnd, concaveEnd])


-- - - - - - - - -  F U N C S  - - - - - - - - - - 


--viewFunc : Func -> Maybe Model.Position -> List Form
--viewFunc fun mParentPos =
--  let getPos block = case mParentPos of
--          Just pPos -> pPos
--          _ -> block.pos
--      display block = [move (floatPos (-90, 0)) block.form]
--  in
--      case fun of
--        P pred -> display pred.block
--        T transform -> display transform.block



--emptyFuncForm : (Int, Int) -> List Form
--emptyFuncForm parentPos =
--  let (fx, fy) = floatPos parentPos
--  in
--      [centered (applyStyle (fromString "function"))
--        |> color bGreen
--        |> size funcWidth funcHeight
--        |> toForm
--        |> move (-90, 0)--(fx , fy )
--        ]


-- - - - - - - - -  R O C K S - - - - - - - - - - 

viewRocks : Rocks -> ID -> Element
viewRocks rocks id =
  let 
  --pos = case mParsentPos of
  --        Just (px, py) -> (px + 60, py + 10)
  --        _ -> rocks.pos
      newRock rock i = viewRock rock
                        |> moveX ((i * rockWidth) - 85)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
      rocksForm rocks = [(List.foldl addRock ([], 0) rocks)
                              |> fst
                              |> group
                              |> move (40, 0)]
      rockBackground = rect rockListWidth rockHeight
                        |> filled white
                        |> move (40, 0)
  in
      collage rockListWidth rockHeight (rockBackground :: rocksForm rocks)
          |> makeHoverable id

viewRock : Rock -> Form
viewRock rock =
  let shape = rockShape rock
      paint = if rock.solid 
        then filled rock.color 
        else outlined (solid rock.color)
  in
      paint shape


rockShape : Rock -> Shape
rockShape rock =
  if| rock.value == 0 -> circle 10.0
    | rock.value == 1 -> rect 3.0 29.0
    | rock.value == 2 -> ngon 3 20
    | otherwise -> ngon rock.value 15.0--((toFloat rock.value) * 1.0)

--emptyRockForm : (Int, Int) -> List Form
--emptyRockForm parentPos =
--  let (fx, fy) = floatPos parentPos
--      fw = hofWidth / 2
--      fh = hofHeight / 2
--      concaveEnd = polygon [(fw, fh)
--                      , (fw + 10, fh)
--                      , (fw + 10, fh/2)
--                      , (fw + 5,  fh/2)
--                      , (fw + 5, -(fh/2))
--                      , ( fw + 10, -(fh/2))
--                      , (fw+10, -fh)
--                      , (fw , -fh)]
--                      |> filled bRed
--  in
--      [concaveEnd, 
--      centered (applyStyle (fromString "rocks"))
--          |> color bBlue
--          |> size rockListWidth rockHeight
--          |> toForm
--          |> move (60, 0 )
--          ]




