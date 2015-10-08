import Model exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (customButton)
import Text exposing (fromString, typeface, style)
import Signal exposing (..)
import Maybe exposing (..)
import List exposing (length)

selectBlock = mailbox Nothing

hofWidth = 70 + funcWidth + rockListWidth
hofHeight = 50
funcWidth = 70
funcHeight = 40
rockListWidth = 10 * rockWidth
rockWidth = 20
rockHeight = 40

bBlue = rgb 66 233 233
bGreen = rgb 66 233 150
bRed = rgb 231 162 233
bPurple = rgb 207 169 213


-- views

applyStyle =style {typeface = ["Courier New"]
          , height = (Just 11.0)
          , color = black
          , bold = True
          , italic = False
          , line = Nothing}

applyTypeface = typeface ["Courier New", "times new roman"]


viewRock : Rock -> Form
viewRock rock =
  let shape = rockShape rock
      paint = if rock.solid then filled rock.color else outlined (solid rock.color)
  in
      paint shape


rockShape : Rock -> Shape
rockShape rock =
  if| rock.value == 0 -> circle 10.0
    | rock.value == 1 -> rect 3.0 29.0
    | rock.value == 2 -> ngon 3 20
    | otherwise -> ngon rock.value 15.0--((toFloat rock.value) * 1.0)


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

dummyModel : Model
dummyModel =
  let
      filter = (H (Filter {id= 1
                          , form = formHOF 1 "filter" bRed (10, 10)
                          , selected = False, pos = (0, 0)}
                          Nothing (Just {pos =(5000, 5000), rockList = dummyRockList})))
      map = (H (Map {id = 2, form = formHOF 2 "map" bPurple (10, 10), selected = False, pos = (100, 100)} Nothing (Just {pos= (1000, 100), rockList = dummyRockList})))
  in
      {nextID = 2
        , blocks = [filter, map]
        }

dummyRockList : List Rock
dummyRockList = [
  {value= 0, solid= True, color = red}
  , {value = 1, solid = False, color = blue}
  , {value = 2, solid = False, color = purple}
  , {value = 3, solid = False, color = red}
  , {value = 4, solid = False, color = purple}
  , {value = 5, solid = True, color = blue}
  , {value = 6, solid = False, color = red}
  , {value = 7, solid = False, color = purple}
  , {value = 8, solid = True, color = red}]

floatPos pos = (toFloat (fst pos), toFloat (snd pos))

--main = collage 700 700 (viewRocks (Just {pos= (-300, 300), rockList= dummyRockList} ))

main = view dummyModel



helperCircles = List.map (\ p -> (move  p (filled green(circle 5.0)))) [(0.0, 0.0)
                                                                        , (-50.0, 0.0)
                                                                        , (50.0, 0.0)
                                                                        , (-100.0, 0.0)
                                                                        , (100.0, 0.0)
                                                                        , (-200.0, 0.0)
                                                                        , (200.0, 0.0)
                                                                        , (0.0, 100.0)
                                                                        , (0.0, -100.0)
                                                                        , (-300.0, 0.0)
                                                                        , (300, 0.0)]

view : Model -> Element
view m =
  collage 700 700 (flattenForest m.blocks)

flattenForest : List Exp -> List Form
flattenForest exps = List.concatMap traverseExp exps

  --List.foldr(\ex accum -> (traverseExp ex) ++ accum) [] exps

traverseExp : Exp -> List Form
traverseExp exp =
  case exp of
    H hof -> traverseHOF hof
--    F func ->
--    R rocks ->

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

viewHOF : Block -> List Form
viewHOF block = [move (floatPos block.pos) block.form]

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



