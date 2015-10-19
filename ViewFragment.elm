module ViewFragment where

import Graphics.Element exposing (leftAligned, centered, color, size, Element, container, midRight)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white, red, black)
import Graphics.Collage exposing (Form, moveX, toForm, rect, outlined, polygon, filled, collage, ngon, circle, solid, Shape, segment, traced, group)
import Model exposing (..)
import Constants exposing (..)

import Drag exposing (makeHoverable)






fragmentToForms : Fragment -> ID -> (List Form, List Form)
fragmentToForms fragment id = frToFormsShift fragment id 0

frToFormsShift : Fragment -> ID -> Int -> (List Form, List Form)
frToFormsShift fr id xShift =
    case fr of
        H hof -> viewHof hof id 0

        F func -> viewFunc func id 0

        E exp -> expToForms exp id 0


expToForms : Exp -> ID -> Int -> (List Form, List Form)
expToForms exp id xShift =
    let
        getHofForms hof = viewHof hof id xShift

        getExpForms compExp = expToForms compExp id (xShift + 1)

        compExpForms hof compExp =
            let 
                hofForms = getHofForms hof
                expForms = getExpForms compExp
            in 
                (fst hofForms ++ fst expForms, snd hofForms ++ snd expForms)
    in

        case exp of
            C hof compExp -> compExpForms hof compExp

            R rocks -> viewRocks rocks id xShift


viewHof : HOF -> ID -> Int -> (List Form, List Form)
viewHof hof id xShift =
    let 
        hofBox str col = (leftAligned (applyStyle (fromString str)))
                      |> color col
                      |> size hofWidth hofHeight
                      |> makeHoverable id
                      |> toForm
                      |> moveX ((hofWidth/2) + (toFloat (xShift * (hofWidth + blockOffset))))

        hofForms col = endForms col xShift

        packageForms mFunc mVRocks str col =
            let 
                funcAttachments = viewMaybeFunc mFunc id xShift
            in 
                (hofBox str col :: fst funcAttachments, hofForms col ++ snd funcAttachments)

    in 
        case hof of 
            Filter mFunc mVRocks -> packageForms mFunc mVRocks "filter" filterColor

            Map mFunc mVRocks -> packageForms mFunc mVRocks "map" mapColor


viewMaybeFunc : Maybe Func -> ID -> Int -> (List Form, List Form)
viewMaybeFunc mFunc id xShift =
    case mFunc of 
        Just func -> viewFunc func id xShift

        Nothing -> viewEmptyFunc id xShift


viewEmptyFunc : ID -> Int -> (List Form, List Form)
viewEmptyFunc id xShift =     
    let 
        el = Graphics.Element.centered (applySmallStyle (fromString "function"))
                        |> color bGreen
                        |> size funcWidth funcHeight
                        |> makeHoverable id
                        |> toForm
                        |> moveX  (((hofWidth/2) + 40) + (toFloat (xShift * (hofWidth +10))))
    in
        ([el], [])



viewFunc : Func -> ID -> Int -> (List Form, List Form)
viewFunc func id xShift =
  let
      funcElement = 
        case func of
          T transform -> viewTransform transform

      makeFunc = (funcElement genericRock)
            |> makeHoverable id
            |> toForm
--            |> moveX  ((hofWidth/2) + (toFloat (xShift * hofWidth)) + 35)
  in
    ([makeFunc], [])



viewTransform : (Rock -> Rock) -> Rock -> Element
viewTransform func rock =
  let 
      whiteSquare = rect (rockWidth + 10 ) (rockWidth+10)
                      |> outlined (dashedLineStyle white)
      whiteArrow = arrowForm
  in
      collage (rockWidth+50) (rockHeight+15) [(viewRock (func rock))]





-- - - - - - - - - - - - E N D S - - - - - - - - - - - - - - - - - -

addRightConcave : Color -> Int -> Int -> Int -> Form
addRightConcave col xShift width height =
  let fw =  (toFloat width) / 2
      fh = (toFloat height) / 2
      offset = (toFloat (xShift* (hofWidth + blockOffset)) + (hofWidth/2))
  in polygon [(fw-1, fh)
                      , (fw + 9, fh )
                      , (fw + 9, fh/2 )
                      , (fw + 4,  fh/2)
                      , (fw + 4, -(fh/2))
                      , ( fw + 9, -(fh/2))
                      , (fw+9, -fh)
                      , (fw - 1, -fh)]
                      |> filled col
                      |> moveX  offset                      

addLeftConvex : Color -> Int -> Int -> Int -> Bool -> Form
addLeftConvex col xShift width height isRocks =
  let fw = (toFloat width) / 2
      fh = (toFloat height) / 2
      shouldAdd = if xShift == 0 then 0 else 1
      offset =  if not isRocks then (toFloat (xShift* (hofWidth + blockOffset)) + (hofWidth/2))
                  else 
                    if xShift == 0 then hofWidth/2  else 
                    (toFloat (xShift * (hofWidth + blockOffset)) +  (rockListWidth/2))
  in
      polygon [(-fw + 1, fh)
                    ,(-fw-5+1 , fh)
                    ,(-fw-5+1, fh/2)
                    ,(-fw-10+1, fh/2)
                    ,(-fw-10+1, -(fh/2))
                    ,(-fw-5+1, -(fh/2))
                    ,(-fw-5+1, -fh)
                    ,(-fw+1, -fh  )
                    ,(-fw+1, -fh ) ]
                        |> filled col
                        |> moveX offset    


endForms col xShift = [addLeftConvex col xShift hofWidth hofHeight False, addRightConcave col xShift hofWidth hofHeight]


-- - - - - - - - -  R O C K S - - - - - - - - - - 

viewRocks : Rocks -> ID -> Int -> (List Form, List Form)
viewRocks rocks id xShift = 
  let 
      newRock rock i = viewRock rock
                        |> moveX ((i * rockWidth) - rockListWidth/2 + (rockWidth/2) + 5)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
      rockOffset = 0


      rocksForm rocks = (List.foldl addRock ([], 0) rocks)
                              |> fst
                              
                              
      background = rect (rockListWidth-3) (rockHeight-3)
                        |> outlined (solid bGreen)

      rockElement = collage (rockListWidth) rockHeight ([background] ++ rocksForm rocks)
                            
      shift = if xShift == 0 then ((hofWidth/2) + (toFloat ((xShift)  * (hofWidth + blockOffset))))
        else (toFloat (xShift)  * (hofWidth + blockOffset) + (rockListWidth/2))
      rockForm = rockElement
                            |> makeHoverable id
                            |> toForm
                            |> moveX shift

  in
      ([rockForm], [addLeftConvex bGreen xShift rockListWidth hofHeight True])

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
    | otherwise -> ngon rock.value 15.0


genericRock : Rock
genericRock = {value= 0, solid= True, color= black }  

arrowForm : Form
arrowForm = 
  let 
      segments =[segment (-30, 0) (20, 0), segment (-30, 0) (-15, 10), segment (-30, 0) (-15, -10)]
  in
      List.map (traced (dashedLineStyle white)) segments
          |> group



