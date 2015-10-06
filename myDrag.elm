import Draggable exposing (..)
import Graphics.Collage exposing (collage, toForm, move, rect, filled, alpha, Form, group, text)
import Graphics.Input exposing (button, clickable)
import Text exposing (fromString)
import Color exposing (lightRed, lightPurple, Color)
import Graphics.Element exposing (..)

--import BlockMenu exposing (..)

type MetaAction = AnAction Action | AnBox BoxTransform

type BoxTransform = Add String | None


boxTransform : Signal.Mailbox BoxTransform
boxTransform = Signal.mailbox None


addButton = 
   (button (Signal.message boxTransform.address (Add "heyyyy")) "add")
   |> toForm
   |> move (-300, 300)

processMetaAction : (Maybe Action -> Model -> Model) -> (BoxTransform -> Model -> Model) -> MetaAction -> Model -> Model
processMetaAction fAction fBox ma m =
  case ma of
    AnAction a -> fAction (Just (a)) m
    AnBox a -> fBox a m




--toAction : MetaAction -> Action
--toAction ma = 
--  case ma of
--    AnAction a -> a
--    _ -> Nothing


updateBoxTransform : BoxTransform -> Model -> Model
updateBoxTransform bt m =
  case bt of
    Add str -> addBox m (makeBox str)
    None -> m

  

fromAction : Action -> MetaAction
fromAction a = AnAction a

fromBoxTransform : BoxTransform -> MetaAction
fromBoxTransform bt = AnBox bt


blockButtonBackground : Color -> Form
blockButtonBackground col =
  rect 60 30
    |> filled col
    |> alpha 0.1


makeClickable : BoxTransform -> Element -> Element
makeClickable ma form = clickable (Signal.message boxTransform.address ma) form

makeBlockButton : (String, Color,  Int) -> Form
makeBlockButton (str, col, i) =
  let 
      clickableText = makeClickable (Add str) (centered (fromString str))
  in
      [blockButtonBackground col, toForm clickableText]
      |> group
      |> move (-300, 300 /10 * (toFloat i))

blockMenu = List.map makeBlockButton [("map", lightRed, 1), ("filter", lightPurple, 2)]


main = 
  let
      combiner = processMetaAction updateDrag updateBoxTransform 
      draggables = makeDraggables (List.map makeBox ["hi", "there", "uhhhh"])
      metaBoxTransform = Signal.map fromBoxTransform boxTransform.signal
      metaDrag = Signal.map fromAction dragSignal
      metaSignal = Signal.merge metaDrag metaBoxTransform
      view model = collage 700 700  ((drawBoxes model) ++ blockMenu)
      modelSignal = Signal.foldp combiner draggables metaSignal
  in
      Signal.map view modelSignal
