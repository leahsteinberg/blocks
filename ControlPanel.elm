module ControlPanel where
import SignalProcessing exposing (evalMailbox)
import Graphics.Collage exposing (..)
import Constants exposing (..)
import Text exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Model exposing (..)

evalButton : (Int, Int) -> Form
evalButton (w, h) =   
    let 
        buttonBackground = (Graphics.Element.color evalColor (centered (fromString "evaluate")))
    in
      customButton (Signal.message evalMailbox.address Forward)
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (toFloat (w//2 - 40), (toFloat (h//2 - 30)))


rewindButton : (Int, Int) -> Form
rewindButton (w, h) = 
    let 
        buttonBackground = (Graphics.Element.color rewindColor (centered (fromString "devaluate")))
    in
      customButton (Signal.message evalMailbox.address Backward)
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (toFloat (w//2 - 40), (toFloat (h//2 - 60)))

evalButtons (w, h) = List.map (\b -> b (w, h)) [rewindButton, evalButton]