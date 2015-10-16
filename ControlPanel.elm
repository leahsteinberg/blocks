module ControlPanel where
import SignalProcessing exposing (evalMailbox)
import Graphics.Collage exposing (..)
import Constants exposing (..)
import Text exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)


evalButton =   
    let 
        buttonBackground = (Graphics.Element.color bGreen (centered (fromString "eval")))
    in
      customButton (Signal.message evalMailbox.address True)
          buttonBackground buttonBackground buttonBackground
            |> toForm
            |> move (200, 250)