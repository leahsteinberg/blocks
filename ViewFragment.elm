module ViewFragment where

import Graphics.Element exposing (leftAligned, centered, color, size, Element, container, midRight)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white, red, black)
import Graphics.Collage exposing (Form, moveX, move, toForm, rect, outlined, polygon, filled, collage, ngon, circle, rotate, solid, Shape, segment, traced, group, dashed)
import Model exposing (..)
import Constants exposing (..)
import Drag exposing (makeHoverable)
import Debug




fragmentToForms : Fragment -> ID -> (List Form, List Form)
fragmentToForms fragment id = if id == -1 then frToFormsShift fragment id -1 else frToFormsShift fragment id 0

frToFormsShift : Fragment -> ID -> Int -> (List Form, List Form)
frToFormsShift fr id xShift =
    case fr of
        H hof -> viewHof hof id xShift

        F func -> viewFunc func id xShift

        E exp -> expToForms exp id xShift


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

        packageFormsWithoutRocks mFunc str col col2 =
            let
                funcAttachments = viewMaybeFunc mFunc id xShift col2
            in 
                (hofBox str col :: fst funcAttachments, hofForms col ++ snd funcAttachments)


        packageForms mFunc vRocks str col col2 =
            let 
                vRockAttachment = [viewVRocks vRocks id xShift]
                funcAttachments = viewMaybeFunc mFunc id xShift col2
            in 
                (hofBox str col :: fst funcAttachments ++ vRockAttachment, hofForms col ++ snd funcAttachments)
--:: hofBox str col :: fst funcAttachments
    in 
        case hof of 
            Filter mFunc mVRocks -> packageForms mFunc mVRocks "filter" filterColor predColor

            Map mFunc mVRocks -> packageForms mFunc mVRocks "map" mapColor transformColor

            Fold mFunc mVRocks -> packageFormsWithoutRocks mFunc "fold" foldColor accumColor


viewMaybeFunc : Maybe Func -> ID -> Int -> Color -> (List Form, List Form)
viewMaybeFunc mFunc id xShift col =
    case mFunc of 
        Just func -> viewFunc func id xShift 

        Nothing -> viewEmptyFunc id xShift col


viewEmptyFunc : ID -> Int -> Color -> (List Form, List Form)
viewEmptyFunc id xShift col =     
    let 
        backgroundCircle = circle (rockWidth)
                      |> outlined (dashedLineStyle col)
      

        circleForm = collage (rockWidth+50) (rockHeight+15) [backgroundCircle]
                            |> makeHoverable id
                            |> toForm
                            |> moveX  (((hofWidth/2) + 40) + (toFloat (xShift * (hofWidth +10))))

    in
        ([circleForm], [])



viewFunc : Func -> ID -> Int -> (List Form, List Form)
viewFunc func id xShift =
  let
      shift = if xShift == -1 then 0 else (((hofWidth/2) + 40) + (toFloat (xShift * (hofWidth +10))))
      scale = if xShift == -1 then 0.57 else 1.0
      funcElement = 
        case func of
          T transform -> viewTransform transform scale
          P pred -> viewPred pred scale
          A accum accRock -> viewAccum accum accRock scale


      makeFunc = (funcElement genericRock)
                        |> makeHoverable id
                        |> toForm
                        |> moveX  shift
  in
    ([makeFunc], [])



predRock : (Rock -> Bool) -> Rock
predRock func =
    let
        maybeDisplayRock = List.foldl 
                (\r mr -> 
                    case mr of 
                        Just predRock -> Just predRock
                        _ -> if func r then Just r else Nothing) 
                        Nothing 
                        predRockList
    in
        case maybeDisplayRock of
            Just displayRock -> displayRock
            _ -> genericRock


viewPred : (Rock -> Bool) -> Float -> Rock -> Element
viewPred func scale rock =
      let 
      backgroundCircle = circle (rockWidth*scale)
                      |> outlined (dashedLineStyle predColor)
      whiteArrow = arrowForm
  in
      collage (rockWidth+50) (rockHeight+15) [backgroundCircle, (viewRock (predRock func) scale)]



viewTransform : (Rock -> Rock) -> Float -> Rock -> Element
viewTransform func scale rock =
  let 
      backgroundCircle = circle (rockWidth*scale)
                      |> outlined (dashedLineStyle transformColor)
      whiteArrow = arrowForm
  in
      collage (rockWidth+50) (rockHeight+15) [backgroundCircle, (viewRock (func rock) scale)]


viewAccum : (Rock -> Rock -> Rock) -> Rock -> Float -> Rock -> Element
viewAccum accum accRock scale rock =
    let
        backgroundCircle = circle (rockWidth * scale)
                                |> outlined (dashedLineStyle accumColor)
    in
        collage (rockWidth + 50) (rockHeight + 15) [backgroundCircle, (viewRock accRock scale)]


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

viewVRocks : Rocks -> ID -> Int ->  Form
viewVRocks rocks id xShift = 
    let shift = if xShift == 0 then ((hofWidth/2) + (toFloat ((xShift)  * (hofWidth + blockOffset)))) - 37 else (toFloat (xShift)  * (hofWidth + blockOffset) + 38)
    in  rockElement rocks 0.38
            |> makeHoverable id
            |> toForm
            |> move (shift, -10)



newRock : Rock -> Int -> Int -> Int -> Float ->  Form
newRock rock i rockWidth rockListWidth scale = viewRock rock scale
                        |> moveX (toFloat ((i * (rockWidth + (4* (floor scale)) ) -  rockListWidth//2 + ( rockWidth)//2) + 5))



rockElement : Rocks -> Float -> Element
rockElement rocks scale =
    let
        rockWidthS = round (rockWidth * scale)
        rockHeightS = round (rockHeight * scale)
        rockListWidthS = rockWidthS * 10
        addRock rock (rList, i) = ((newRock rock i rockWidthS rockListWidthS scale) :: rList, i + 1)
        rocksForm rocks = (List.foldl addRock ([], 0) rocks)
                              |> fst      
        background = rect (toFloat (rockListWidthS-3)) (toFloat (rockHeightS-3))
                        |> outlined (solid rocksColor)

      in
            collage (rockListWidth) rockHeight ([background] ++ rocksForm rocks)



viewRocks : Rocks -> ID -> Int -> (List Form, List Form)
viewRocks rocks id xShift = 
  let 
                     
      shift = if xShift == 0 then ((hofWidth/2) + (toFloat ((xShift)  * (hofWidth + blockOffset)))) else (toFloat (xShift)  * (hofWidth + blockOffset) + (rockListWidth/2))
      rockForm = rockElement rocks 1.0
                            |> makeHoverable id
                            |> toForm
                            |> moveX shift

  in
      ([rockForm], [addLeftConvex rocksColor xShift rockListWidth hofHeight True])

viewRock : Rock -> Float ->  Form
viewRock rock scale =
    let 
        shape = rockShape rock scale
        paint = if rock.solid 
                then filled rock.color 
                else outlined (solid rock.color)
    in
        if rock.value == 2 then xForm  scale rock.color rock.solid else 
            if rock.value % 7 ==3 then rotate 100 (paint shape) else paint shape



rockShape : Rock -> Float -> Shape
rockShape rock scale =
    let 
        value = rock.value % 7
    in
        if  | value == 0 -> circle (10.0 * scale)
            | value == 1 -> rect 3.0 (26.0 * scale)
            | value == 2 -> ngon value (12.0 * scale)
            | value == 3 -> ngon value (13.0 * scale)
            | otherwise -> ngon value (13.0 * scale)


genericRock : Rock
genericRock = {value= 0, solid= True, color= black }  

arrowForm : Color -> Form
arrowForm col = 
  let 
      segments =[segment (-30, 0) (20, 0), segment (-30, 0) (-15, 10), segment (-30, 0) (-15, -10)]
  in
      List.map (traced (dashedLineStyle white)) segments
          |> group

xForm : Float -> Color -> Bool -> Form
xForm scale col filledIn = 
  let 
      lineStyle = if filledIn then solid else dashedLineStyle
      segments =[segment (-10*scale, 10*scale) (10*scale, -10*scale), segment (-10*scale, -10*scale) (10*scale, 10*scale)]
  in
      List.map (traced (lineStyle col)) segments
          |> group


