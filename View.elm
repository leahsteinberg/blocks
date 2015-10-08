module View where 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (leftAligned, centered, color, size)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white)

-- my modules
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing(..)



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
      makeHOFView block mFunc mRocks = viewHOF block
                                        ++ maybeFuncForm mFunc block.pos
                                        ++ maybeRocksForm mRocks block.pos

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
  in
      customButton (Signal.message selectBlock.address (Just id))
      background background background
        |> toForm
        |> move (floatPos pos)

viewHOF : Block -> List Form
viewHOF block = [move (floatPos block.pos) block.form]



-- - - - - - - - -  F U N C S  - - - - - - - - - - 


viewFunc : Func -> Maybe Model.Position -> List Form
viewFunc fun mParentPos =
  let getPos block = case mParentPos of
          Just pPos -> pPos
          _ -> block.pos
      display block = [move (floatPos (getPos block)) block.form]
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
        |> move (fx- funcWidth-10, fy+10)--(fx , fy )
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
                              |> move (floatPos pos)]
      rockBackground = rect rockListWidth rockHeight
                        |> filled white
                        |> move (floatPos pos)
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
          |> move (fx + 60, fy + 10)
          ]




