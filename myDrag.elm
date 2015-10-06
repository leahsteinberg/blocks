import Draggable exposing (..)
import Graphics.Collage exposing (collage, toForm, move)
import Graphics.Input exposing (button)

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



--boxTransform.signal


main = 
  let
      combiner = processMetaAction updateDrag updateBoxTransform 
      draggables = makeDraggables (List.map makeBox ["hi", "there", "uhhhh"])
      metaBoxTransform = Signal.map fromBoxTransform boxTransform.signal
      metaDrag = Signal.map fromAction dragSignal
      metaSignal = Signal.merge metaDrag metaBoxTransform
      view model = collage 700 700  (addButton :: (drawBoxes model))
      modelSignal = Signal.foldp combiner draggables metaSignal
  in
      Signal.map view modelSignal
