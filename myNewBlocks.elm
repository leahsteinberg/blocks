import Model exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (customButton)
import Text exposing (fromString)
import Signal exposing (..)
import Maybe exposing (..)

selectBlock = mailbox False

formHOF : Int -> String -> Color -> (Int, Int) -> Form
formHOF id str col (x, y) =
  let
      background = (centered (fromString str))
                      |> color col
                      |> size 60 20
  in
      customButton (Signal.message selectBlock.address True)
      background background background
        |> toForm
        |> move (toFloat x, toFloat y)



dummyModel : Model
dummyModel =
  let
      filter = (Filter {id= 1, form= formHOF 1 "filter" lightRed (10, 10), selected= False, pos= (10, 10)} Nothing Nothing)
  in
      {nextID = 2
            , blocks = [H filter ]
            }


main = collage 700 700 (viewRocks (Just [
  {value= 0, solid= True, color = red}
  , {value = 1, solid = False, color = blue}
  , {value = 2, solid = False, color = green}
  , {value = 4, solid = False, color = red}
  , {value = 5, solid = True, color = green}
  , {value = 6, solid = True, color = blue}]))

--main = view dummyModel

--view : Model -> Form
--view m =
--  collage 700 700

flattenForest : List Exp -> List Form
flattenForest exps = List.foldr(\ex accum -> (traverseExp ex) ++ accum) [] exps

traverseExp : Exp -> List Form
traverseExp exp =
  case exp of
    H hof -> traverseHOF hof

traverseHOF : HOF -> List Form
traverseHOF hof =
  case hof of
    Filter block mPred mRocks -> block.form :: viewFunc mPred ++ viewRocks mRocks
    Map block mTransform mRocks -> block.form :: viewFunc mTransform ++ viewRocks mRocks

viewFunc : Maybe Func -> List Form
viewFunc mFun =
  case mFun of
    Just fun ->
      case fun of
        P pred -> [pred.block.form]
        T transform -> [transform.block.form]
    _ -> []

viewRocks : Maybe RockList -> List Form
viewRocks mRocks =
  let newRock rock i = viewRock rock
                        |> moveX (i * 13.0)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
  in
      case mRocks of
          Just rocks -> fst (List.foldl addRock ([], 0) rocks)
          _ -> []

viewRock : Rock -> Form
viewRock rock =
  let shape = rockShape rock
      paint = if rock.solid then filled rock.color else outlined (solid rock.color)
  in
      paint shape


rockShape : Rock -> Shape
rockShape rock =
  if| rock.value == 0 -> circle 9.0
    | rock.value == 1 -> rect 3.0 16.0
    | rock.value == 2 -> ngon 3 20
    | otherwise -> ngon rock.value 30




