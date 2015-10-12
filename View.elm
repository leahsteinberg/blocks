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

expToElsAndForms : Exp -> ID -> (List Element, List Form)
expToElsAndForms exp id = expToElementsShift exp id 1



expToElementsShift : Exp -> ID -> Int -> (List Element, List Form)
expToElementsShift exp id xShift =  
  case exp of
    H hof -> hofToElements hof id xShift
    RE rockExp -> 
      case rockExp of
        R rocks -> viewRocks rocks id xShift
        Higher hof -> hofToElements hof id xShift
--    F func -> funcToElements func
--    R rockExp -> rocksToElements rockExp

hofToElements : HOF -> ID -> Int -> (List Element, List Form)
hofToElements hof id xShift =
  let 
      --clearElement = spacer (hofWidth+10) hofHeight
      hofElForContainer str col = --(collage (hofWidth*xShift*2) hofHeight )
                      (leftAligned (applyStyle (fromString str)))
                      |> color col
                      |> size hofWidth hofHeight
      hofEl str col = container (xShift*(2*(hofWidth +10))) hofHeight midRight (hofElForContainer str col)
                     -- |>toForm
                      --|> moveX (toFloat xShift*(hofWidth+10))
                      --])
                      
                      |> makeHoverable id
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


hofAttachments : Maybe Func -> Maybe RockExpression -> ID -> Color -> Int -> (List Element, List Form)
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
      

funcToElement : Func -> ID -> Element
funcToElement func id =
  (leftAligned (applyStyle (fromString "filled func")))
                      |> color bGreen
                      |> size funcWidth funcHeight
                      |> makeHoverable id




emptyFunc : Color -> ID -> Int -> Element
emptyFunc col id xShift = 
  let emptyF = Graphics.Element.centered (applySmallStyle (fromString "function"))
                        |> color bGreen
                        |> size funcWidth funcHeight
  in
      (container (xShift*(2*(hofWidth +10))) 100 midRight emptyF)
                                |> makeHoverable id




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
      offset = 
        if |xShift == 0 -> 0
            |xShift == 1 -> 2
            |xShift == 2 -> 11.5
            |xShift == 3 -> 22
            |otherwise -> 7 * xShift
  in polygon [(fw-1, fh)
                      , (fw + 9, fh)
                      , (fw + 9, fh/2)
                      , (fw + 4,  fh/2)
                      , (fw + 4, -(fh/2))
                      , ( fw + 9, -(fh/2))
                      , (fw+9, -fh)
                      , (fw - 1, -fh)]
                      |> filled col
                      |> moveX ((toFloat ((84) + ((xShift-1) *hofWidth))) +  offset)                        
--                      --|> moveX (toFloat ((xShift*(hofWidth))))  

addLeftConvex : Color -> Int -> Int -> Int -> Form
addLeftConvex col xShift width height =
  let fw = (toFloat width) / 2
      fh = (toFloat height) / 2
      offset = 
        if |xShift == 0 -> 0
            |xShift == 1 -> 2
            |xShift == 2 -> 11.5
            |xShift == 3 -> 22
            |xShift == 4 -> 58
            |otherwise -> 7 * xShift

  in
      polygon [(-fw + 1, fh)
                    ,(-fw-5+1 , fh)
                    ,(-fw-5+1, fh/2)
                    ,(-fw-10+1, fh/2)
                    ,(-fw-10+1, -(fh/2))
                    ,(-fw-5+1, -(fh/2))
                    ,(-fw-5+1, -fh)
                    ,(-fw+1, -fh)
                    ,(-fw+1, fh) ]
                        |> filled col
                        |> moveX ((toFloat ((84) + ((xShift-1) *hofWidth))) +  offset)       
--((toFloat ((84) + ((xShift-1) *hofWidth))) +  (1.9 * (toFloat xShift))) 

                          --(xShift*(hofWidth))+ )) 
--                          --- (xShift *hofWidth//2) + 10))



-- - - - - - - - -  A  P  I  - - - - - - - - - - - - - - - 

endForms col xShift = [addLeftConvex col xShift hofWidth hofHeight, addRightConcave col xShift hofWidth hofHeight]




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

viewRocks : Rocks -> ID -> Int -> (List Element, List Form)
viewRocks rocks id xShift =
  let 
      newRock rock i = viewRock rock
                        |> moveX (i * rockWidth)
      addRock rock (rList, i) = ((newRock rock i) :: rList, i + 1)
      rocksForm rocks = [(List.foldl addRock ([], 0) rocks)
                              |> fst
                              |> group

                              ]
      background = rect (rockListWidth-3) (rockHeight-3)
                        |> outlined (solid bGreen)
                        |> moveX (toFloat 86)

      rockElement = collage (rockListWidth*2) rockHeight (background ::(rocksForm rocks))
                            
      shift = if xShift == 0 then rockListWidth else ((xShift)*2 *(hofWidth))+rockListWidth

      rockInContainer = container shift hofHeight midRight rockElement
                            |> makeHoverable id
-----                        |> moveX (toFloat ((xShift)*(hofWidth+10)))

      --outlineRect = rect (rockListWidth+100) (rockHeight+100)
      --                    |> outlined (Constants.lineStyle bGreen)
  in
      ([rockInContainer]
          ,
          [addLeftConvex bGreen xShift rockListWidth hofHeight])

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




