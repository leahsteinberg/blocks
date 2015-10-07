import Model exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (customButton)
import Text exposing (fromString)
import Signal exposing (..)
import Maybe exposing (..)

selectBlock = mailbox Nothing

formHOF : Int -> String -> Color -> (Int, Int) -> Form
formHOF id str col (x, y) =
  let
      background = (leftAligned (fromString str))
                      |> color col
                      |> size 120 20
  in
      customButton (Signal.message selectBlock.address (Just 1))
      background background background
        |> toForm
        |> move (toFloat x, toFloat y)

emptyPredForm : (Int, Int) -> Form
emptyPredForm pos =
  let (fx, fy) = floatPos pos
  in
      leftAligned (fromString "pred")
        |> color red
        |> size 40 15
        |> move (fx + 30, fy)

dummyModel : Model
dummyModel =
  let
      filter = (H (Filter {id= 1, form = formHOF 1 "filter" rgbLightRed (10, 10), selected= False, pos= (10, 10)} Nothing Nothing))
  in
      {nextID = 2
            , blocks = [filter ]
            }
rgbLightRed = rgb 213 169 181

dummyRockList : List Rock
dummyRockList = [
  {value= 0, solid= True, color = red}
  , {value = 1, solid = False, color = blue}
  , {value = 2, solid = False, color = green}
  , {value = 4, solid = False, color = red}
  , {value = 5, solid = False, color = green}
  , {value = 6, solid = True, color = blue}
  , {value = 7, solid = False, color = red}
  , {value = 8, solid = False, color = green}
  , {value = 9, solid = True, color = red}]

floatPos pos = (toFloat (fst pos), toFloat (snd pos))

--main = collage 700 700 (viewRocks (Just {pos= (-300, 300), rockList= dummyRockList} ))

main = view dummyModel

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
  let maybePredForm mPred block = 
      case mPred of
        Just pred -> viewFunc pred
        _ -> emptyPredForm block.pos
  in
      case hof of
        Filter block mPred mRocks -> block.form :: maybePredForm mPred block.pos ++ viewRocks mRocks
        Map block mTransform mRocks -> block.form :: viewFunc mTransform ++ viewRocks mRocks


viewHOF : Block -> Form
viewHOF block = move (floatPos block.pos) block.form

viewFunc : Func -> List Form
viewFunc fun =
  case fun of
    P pred -> [pred.block.form]
    T transform -> [transform.block.form]


viewRocks : Maybe Rocks -> List Form
viewRocks mRocks =
  let newRock rock i = viewRock rock
                        |> moveX (i * 40)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
      rocksForm rocks = [(List.foldl addRock ([], 0) rocks.rockList)
                              |> fst
                              |> group
                              |> move (floatPos rocks.pos)]
  in
      case mRocks of
          Just rocks -> rocksForm rocks
          _ -> []

viewRock : Rock -> Form
viewRock rock =
  let shape = rockShape rock
      paint = if rock.solid then filled rock.color else outlined (solid rock.color)
  in
      paint shape


rockShape : Rock -> Shape
rockShape rock =
  if| rock.value == 0 -> circle 13.0
    | rock.value == 1 -> rect 3.0 29.0
    | rock.value == 2 -> ngon 3 20
    | otherwise -> ngon rock.value 15.0--((toFloat rock.value) * 1.0)




