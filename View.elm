module View where 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (leftAligned, centered, color, size)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white, red)

-- my modules
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing(..)
import Drag exposing (makeHoverable)



-- - - - - - - - -  T R A V E R S A L S  - - - - - - - - - - 
flattenForest : List Exp -> List Form
flattenForest exps = List.concatMap traverseExp exps


traverseExp : Exp -> List Form
traverseExp exp =
  case exp of
    H hof -> traverseHOF hof
--    F func ->
--    R rocks ->


-- - - - - - - - -  Higher Order Functions  - - - - - - - - - - 


traverseHOF : HOF -> List Form
traverseHOF hof =
  let
      makeHOFView block mFunc mRocks = [(group (viewHOF block
                                        ++ maybeFuncForm mFunc block.pos
                                        ++ maybeRocksForm mRocks block.pos))
                                        |> move (floatPos block.pos)]

      maybeRocksForm maybeRocks parentPos =
      case maybeRocks of
        Just rocks -> viewRocks rocks (Just parentPos)
        _ -> emptyRockForm parentPos

      maybeFuncForm maybeFunc parentPos =
      case maybeFunc of
        Just func -> viewFunc func (Just parentPos)
        _ -> emptyFuncForm parentPos
  in
      case hof of
        Filter block mPred mRocks -> makeHOFView block mPred mRocks

        Map block mTransform mRocks -> makeHOFView block mTransform mRocks


formHOF : Int -> String -> Color -> Model.Position -> Form
formHOF id str col pos =
  let
      background = (leftAligned (applyStyle (fromString str)))
                      |> color col
                      |> size hofWidth hofHeight
      (x, y) = floatPos pos
      fw = hofWidth / 2
      fh = hofHeight / 2
      poly = polygon [(x + fh, y + fh+10)
                                , (x+ fw + 5, y + fh)
                                , (x + fw + 5, y+ fh/2)
                                , (x + fw, y + fh/2)
                                , (x + fw, y - (fh/2))
                                , (x + fw + 5, y - (fh/2))
                                , (x + fw + 5, y - fh)
                                , (x  + fw, y - fh)
                                , (x+ fw , y + fh-10)]
                                |> filled red
  in
      
      group ((customButton (Signal.message selectBlock.address (Just id))
      background background background
        |> makeHoverable id
        |> toForm) :: [poly])
------|> move (floatPos pos)

viewHOF : Block -> List Form
viewHOF block = [ 
--  move (floatPos block.pos) 
    block.form]



-- - - - - - - - -  F U N C S  - - - - - - - - - - 


viewFunc : Func -> Maybe Model.Position -> List Form
viewFunc fun mParentPos =
  let getPos block = case mParentPos of
          Just pPos -> pPos
          _ -> block.pos
      display block = [move (floatPos (-10, 0)) block.form]
  in
      case fun of
        P pred -> display pred.block
        T transform -> display transform.block



emptyFuncForm : (Int, Int) -> List Form
emptyFuncForm parentPos =
  let (fx, fy) = floatPos parentPos
  in
      [centered (applyStyle (fromString "function"))
        |> color bGreen
        |> size funcWidth funcHeight
        |> toForm
        |> move (-10, 0)--(fx , fy )
        ]


-- - - - - - - - -  R O C K S - - - - - - - - - - 

viewRocks : Rocks -> Maybe Model.Position -> List Form
viewRocks rocks mParentPos =
  let pos = case mParentPos of
          Just (px, py) -> (px + 60, py + 10)
          _ -> rocks.pos
      newRock rock i = viewRock rock
                        |> moveX ((i * rockWidth) - 85)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
      rocksForm rocks = [(List.foldl addRock ([], 0) rocks.rockList)
                              |> fst
                              |> group
                              |> move (40, 0)]
      rockBackground = rect rockListWidth rockHeight
                        |> filled white
                        |> move (40, 0)
  in
      rockBackground :: rocksForm rocks

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

emptyRockForm : (Int, Int) -> List Form
emptyRockForm parentPos =
  let (fx, fy) = floatPos parentPos
  in
      [centered (applyStyle (fromString "rocks"))
          |> color bBlue
          |> size rockListWidth rockHeight
          |> toForm
          |> move (60, 0 )
          ]




