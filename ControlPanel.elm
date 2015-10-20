module ControlPanel where
import SignalProcessing exposing (evalMailbox)
import Graphics.Collage exposing (..)
import Constants exposing (..)
import Text exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Model exposing (..)

evalButton : Int -> Form
evalButton w =   
    let 
        buttonBackground = (Graphics.Element.color evalColor (centered (fromString "eval")))
    in
      customButton (Signal.message evalMailbox.address Forward)
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (toFloat (w//2 - 40), 250)


rewindButton : Int -> Form
rewindButton w = 
    let 
        buttonBackground = (Graphics.Element.color rewindColor (centered (fromString "rewind")))
    in
      customButton (Signal.message evalMailbox.address Backward)
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (toFloat (w//2 - 40), 200)

evalButtons w = List.map (\b -> b w) [rewindButton, evalButton]