import Model exposing (..)
import Constants exposing (..)

fragmentToForms : Fragment -> ID -> (List Form, List Form)
fragmentToForms fragment id = frToFormsShift fragment id 0

frToFormsShift : Fragment -> ID -> Int -> (List Form, List Form)
frToFormsShift fr id xShift =
    case fr of
        H hof -> viewHof hof id 0

        F func -> viewFunc func id 0

        E exp -> expToForms id 0


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
                (fst hofForms ++ fst expForms, snd hofForms ++ fst expForms)

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
                (hofBox str col :: fst funcAttachments, hofForms col :: snd funcAttachments)

    in 
        case hof of 
            Filter mFunc mVRocks -> packageForms mFunc mVRocks "filter" filterColor

            Map mFunc mVRocks -> packageForms mFunc mVRocks "map" mapColor


viewMaybeFunc : Maybe Func -> ID -> Int -> (List Form, List Form)
viewMaybeFunc mFunc id xShift =
    case mFunc of 
        Just (Fun func) -> viewFunc func id xShift

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
viewFunc func id xShift = ([], [])





-- - - - - - - - - - - - E N D S - - - - - - - - - - - - - - - - - -

addRightConcave : Color -> Int -> Int -> Int -> Form
addRightConcave col xShift width height =
  let fw =  (toFloat width) / 2
      fh = (toFloat height) / 2
      offset = (toFloat (xShift* (hofWidth + blockOffset)) + (hofWidth/2))
  in polygon [(fw-1, fh)
                      , (fw + 9, fh )
                      , (fw + 9, fh/2)
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
                    ,(-fw+1, -fh )
                    ,(-fw+1, -fh) ]
                        |> filled col
                        |> moveX offset    


endForms col xShift = [addLeftConvex col xShift hofWidth hofHeight False, addRightConcave col xShift hofWidth hofHeight]




