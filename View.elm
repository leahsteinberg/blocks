module View where 


x = 4
--import Graphics.Collage exposing (..)
--import Graphics.Element exposing (leftAligned, centered, color, size, Element, container, midRight)
--import Text exposing (fromString)
--import Graphics.Input exposing (customButton)
--import Color exposing (Color, white, red, black)

---- my modules
--import Constants exposing (..)
--import Model exposing (..)
----import SignalProcessing exposing(..)
--import Drag exposing (makeHoverable)


--genericRock : Rock
--genericRock = {value= 0, solid= True, color= black }  

--arrowForm : Form
--arrowForm = 
--  let 
--      segments =[segment (-30, 0) (20, 0), segment (-30, 0) (-15, 10), segment (-30, 0) (-15, -10)]
--  in
--      List.map (traced (dashedLineStyle white)) segments
--          |> group

---- - - - - - - - -  N E W  -  S T U F F - - - - - - - - 

--expToElsAndForms : Exp -> ID -> (List Form, List Form)
--expToElsAndForms exp id = expToElementsShift exp id 0



--expToElementsShift : Exp -> ID -> Int -> (List Form, List Form)
--expToElementsShift exp id xShift =  
--  case exp of
--    H hof -> hofToElements hof id xShift
--    RE rockExp -> 
--      case rockExp of
--        R rocks -> viewRocks rocks id xShift
--        Higher horderfunc -> hofToElements horderfunc id xShift


--hofToElements : HOF -> ID -> Int -> (List Form, List Form)
--hofToElements hof id xShift = 
--  let 
--      hofEl str col = (leftAligned (applyStyle (fromString str)))
--                      |> color col
--                      |> size hofWidth hofHeight
--                      |> makeHoverable id
--                      |> toForm
--                      |> moveX ((hofWidth/2) + (toFloat (xShift * (hofWidth + blockOffset))))
                      
                      
--      hofForms col = endForms col xShift
--  in
--      case hof of
--        Filter mFunc mRockExp -> 
--          let 
--              attachments = hofAttachments mFunc mRockExp id bRed xShift
--          in
--              (hofEl "filter" bRed :: (fst attachments), (hofForms bRed)++ (snd attachments))
--        Map mFunc mRockExp -> 
--          let
--              attachments = hofAttachments mFunc mRockExp id bBlue xShift
--          in
--              (hofEl "map" bBlue :: (fst attachments), (hofForms bBlue)++ (snd attachments))

--hofAttachments : Maybe Func -> Maybe RockExpression -> ID -> Color -> Int -> (List Form, List Form)
--hofAttachments mFunc mRE id col xShift =
--  let 
--      funcElements = 
--        case mFunc of
--          Just func -> ([funcToElement func id xShift], [])
--          Nothing -> ([emptyFunc col id xShift], [])

--      rockExpElements =
--        case mRE of
--          Just re -> expToElementsShift (RE re) id (xShift+1)
--          Nothing -> ([], [])

--  in
--      ((fst funcElements) ++ (fst rockExpElements), (snd funcElements) ++ (snd rockExpElements))
      

--funcToElement : Func -> ID -> Int -> Form
--funcToElement func id xShift =
--  let
--      viewFunc = 
--        case func of
--          T transform -> viewTransform transform
--  in
--      (viewFunc genericRock)
--            |> makeHoverable id
--            |> toForm
--            |> moveX  ((hofWidth/2) + (toFloat (xShift * hofWidth )) + 35)


--viewTransform : (Rock -> Rock) -> Rock -> Element
--viewTransform func rock =
--  let 
--      whiteSquare = rect (rockWidth + 10 ) (rockWidth+10)
--                      |> outlined (dashedLineStyle white)
--      whiteArrow = arrowForm
--  in
--      collage (rockWidth+50) (rockHeight+15) [arrowForm, (viewRock (func rock))]




--emptyFunc : Color -> ID -> Int -> Form
--emptyFunc col id xShift = 
--  Graphics.Element.centered (applySmallStyle (fromString "function"))
--                        |> color bGreen
--                        |> size funcWidth funcHeight
--                        |> makeHoverable id
--                        |> toForm
--                        |> moveX  (((hofWidth/2) + 40) + (toFloat (xShift * (hofWidth +10))))


---- - - - - - - - - - - - E N D S - - - - - - - - - - - - - - - - - -

--addRightConcave : Color -> Int -> Int -> Int -> Form
--addRightConcave col xShift width height =
--  let fw =  (toFloat width) / 2
--      fh = (toFloat height) / 2
--      offset = (toFloat (xShift* (hofWidth + blockOffset)) + (hofWidth/2))
--  in polygon [(fw-1, fh)
--                      , (fw + 9, fh )
--                      , (fw + 9, fh/2)
--                      , (fw + 4,  fh/2)
--                      , (fw + 4, -(fh/2))
--                      , ( fw + 9, -(fh/2))
--                      , (fw+9, -fh)
--                      , (fw - 1, -fh)]
--                      |> filled col
--                      |> moveX  offset                      

--addLeftConvex : Color -> Int -> Int -> Int -> Bool -> Form
--addLeftConvex col xShift width height isRocks =
--  let fw = (toFloat width) / 2
--      fh = (toFloat height) / 2
--      shouldAdd = if xShift == 0 then 0 else 1
--      offset =  if not isRocks then (toFloat (xShift* (hofWidth + blockOffset)) + (hofWidth/2))
--                  else 
--                    if xShift == 0 then hofWidth/2  else 
--                    (toFloat (xShift * (hofWidth + blockOffset)) +  (rockListWidth/2))
--  in
--      polygon [(-fw + 1, fh)
--                    ,(-fw-5+1 , fh)
--                    ,(-fw-5+1, fh/2)
--                    ,(-fw-10+1, fh/2)
--                    ,(-fw-10+1, -(fh/2))
--                    ,(-fw-5+1, -(fh/2))
--                    ,(-fw-5+1, -fh)
--                    ,(-fw+1, -fh )
--                    ,(-fw+1, -fh) ]
--                        |> filled col
--                        |> moveX offset    


--endForms col xShift = [addLeftConvex col xShift hofWidth hofHeight False, addRightConcave col xShift hofWidth hofHeight]


---- - - - - - - - -  R O C K S - - - - - - - - - - 

--viewRocks : Rocks -> ID -> Int -> (List Form, List Form)
--viewRocks rocks id xShift = 
--  let 
--      newRock rock i = viewRock rock
--                        |> moveX ((i * rockWidth) - rockListWidth/2 + (rockWidth/2) + 5)
--      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
--      rockOffset = 0


--      rocksForm rocks = (List.foldl addRock ([], 0) rocks)
--                              |> fst
                              
                              
--      background = rect (rockListWidth-3) (rockHeight-3)
--                        |> outlined (solid bGreen)

--      rockElement = collage (rockListWidth) rockHeight ([background] ++ rocksForm rocks)
                            
--      shift = if xShift == 0 then ((hofWidth/2) + (toFloat ((xShift)  * (hofWidth + blockOffset))))
--        else (toFloat (xShift)  * (hofWidth + blockOffset) + (rockListWidth/2))
--      rockForm = rockElement
--                            |> makeHoverable id
--                            |> toForm
--                            |> moveX shift

--  in
--      ([rockForm], [addLeftConvex bGreen xShift rockListWidth hofHeight True])

--viewRock : Rock -> Form
--viewRock rock =
--  let shape = rockShape rock
--      paint = if rock.solid 
--        then filled rock.color 
--        else outlined (solid rock.color)
--  in
--      paint shape


--rockShape : Rock -> Shape
--rockShape rock =
--  if| rock.value == 0 -> circle 10.0
--    | rock.value == 1 -> rect 3.0 29.0
--    | rock.value == 2 -> ngon 3 20
--    | otherwise -> ngon rock.value 15.0



