module View where 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (leftAligned, centered, color, size, Element, container, midRight)
import Text exposing (fromString)
import Graphics.Input exposing (customButton)
import Color exposing (Color, white, red)

-- my modules
import Constants exposing (..)
import Model exposing (..)
import SignalProcessing exposing(..)
import Drag exposing (makeHoverable)





-- - - - - - - - -  N E W  -  S T U F F - - - - - - - - 

expToElsAndForms : Exp -> ID -> (List Form, List Form)
expToElsAndForms exp id = expToElementsShift exp id 0



expToElementsShift : Exp -> ID -> Int -> (List Form, List Form)
expToElementsShift exp id xShift =  
  case exp of
    H hof -> hofToElements hof id xShift
    RE rockExp -> 
      case rockExp of
        R rocks -> viewRocks rocks id xShift
        Higher horderfunc -> hofToElements horderfunc id xShift
--    F func -> funcToElements func
--    R rockExp -> rocksToElements rockExp

hofToElements : HOF -> ID -> Int -> (List Form, List Form)
hofToElements hof id xShift = 
  let 
      --clearElement = spacer (hofWidth+10) hofHeight
      hofEl str col = (leftAligned (applyStyle (fromString str)))
                      |> color col
                      |> size hofWidth hofHeight
                      |> makeHoverable id
                      |> toForm
                      |> moveX ((hofWidth/2) + (toFloat (xShift * (hofWidth + blockOffset))))
--      hofElWithContainer str col = container ((hofWidth/2) + (xShift* hofWidth)) hofHeight midRight (hofEl str col)
--(xShift*(2*(hofWidth +10))) 
                     -- |>toForm
                      --|> moveX (toFloat xShift*(hofWidth+10))
                      --])
                      
                      
      hofForms col = endForms col xShift
  in
      case hof of
        Filter mFunc mRockExp -> 
          let 
              attachments = hofAttachments mFunc mRockExp id bRed xShift
          in
              (hofEl "filter" bRed :: (fst attachments), (hofForms bRed)++ (snd attachments))
        Map mFunc mRockExp -> 
          let
              attachments = hofAttachments mFunc mRockExp id bBlue xShift
          in
              (hofEl "map" bBlue :: (fst attachments), (hofForms bBlue)++ (snd attachments))

hofAttachments : Maybe Func -> Maybe RockExpression -> ID -> Color -> Int -> (List Form, List Form)
hofAttachments mFunc mRE id col xShift =
  let 
      funcElements = 
        case mFunc of
          Just func -> ([funcToElement func id], [])
          Nothing -> ([emptyFunc col id xShift], [])

      rockExpElements =
        case mRE of
          Just re -> expToElementsShift (RE re) id (xShift+1) ---TODO
            --case re of
            --Higher hof -> expToElementsShift (hof id (xShift + hofWidth)
            --R rockExp -> expToElementsShift rockExp id (xShift + hofWidth)
          Nothing -> ([], [])

  in
      ((fst funcElements) ++ (fst rockExpElements), (snd funcElements) ++ (snd rockExpElements))
      

funcToElement : Func -> ID -> Form
funcToElement func id =
  (leftAligned (applyStyle (fromString "filled func")))
                      |> color bGreen
                      |> size funcWidth funcHeight
                      |> makeHoverable id
                      |> toForm




emptyFunc : Color -> ID -> Int -> Form
emptyFunc col id xShift = 
  Graphics.Element.centered (applySmallStyle (fromString "function"))
                        |> color bGreen
                        |> size funcWidth funcHeight
                        |> makeHoverable id
                        |> toForm
                        |> moveX  ((hofWidth/2) + (toFloat (xShift * hofWidth )) + 35)


  --let emptyF = Graphics.Element.centered (applySmallStyle (fromString "function"))
  --                      |> color bGreen
  --                      |> size funcWidth funcHeight
  --in
  --    (container (xShift*(2*(hofWidth +10))) 100 midRight emptyF)
  --                              |> makeHoverable id




--hofElementWithFunc : HOF -> Func -> ID -> String -> Color -> Int -> List Element
--hofElementWithFunc hof func id str col xShift =
--  let 
--      funcString  =       
--          case func of
--            P pred -> "pred"
--            T transform -> "transform"
--      funcElement funcStr =
--            leftAligned (applyStyle (fromString funcStr))
--                        |> color bGreen
--                        |> size funcWidth funcHeight

--  in
--      hofElementNoFunc id str col xShift ++ [funcElement funcString]


--hofElementNoFunc : HOF -> ID -> String -> Color -> Int -> List Element
--hofElementNoFunc hof id str col xShift =
--  let 
--      emptyFunc = Graphics.Element.centered (applySmallStyle (fromString "function")
--                        |> color bGreen
--                        |> size funcWidth funcHeight
                        

--      funcInContainer = (container 130 100 Graphics.Element.midRight emptyFunc)
--                                |> makeHoverable id
--      hof = (leftAligned (applyStyle (fromString str)))
--                      |> color col
--                      |> size hofWidth hofHeight
--                      |> makeHoverable id
--  in
--      case hof of

--    [hof, funcInContainer]

      --[(leftAligned (applyStyle (fromString str)))
      --                |> color col
      --                |> size hofWidth hofHeight
      --                |> makeHoverable id,
      --                emptyFunc]




-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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



-- - - - - - - - -  A  P  I  - - - - - - - - - - - - - - - 

endForms col xShift = [addLeftConvex col xShift hofWidth hofHeight False, addRightConcave col xShift hofWidth hofHeight]




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
      ([rockForm]
          ,
          [addLeftConvex bGreen xShift rockListWidth hofHeight True])

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




