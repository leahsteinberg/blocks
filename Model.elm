module Model where


import Color exposing (Color)
import Maybe exposing (Maybe)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (Form)
import Dict exposing (Dict)

type alias Model = {nextID: ID, blocks: Dict ID Block}

-- not composable
type Fragment = E Exp | F Func | H HOF

-- composable
type Exp = C HOF Exp
            | R Rocks


type HOF = Filter (Maybe Func) (Maybe Rocks) | Map (Maybe Func) (Maybe Rocks)



type alias Rocks = List Rock

type alias Rock = {value: Int
            , solid: Bool
            , color: Color}


type Func  = P Pred | T Transform | A Accum 

type alias Pred = (Rock -> Bool)

type alias Transform = (Rock -> Rock)

type alias Accum = (Rock -> Rocks -> Rocks)

type alias Block = {id: ID
                    , ele: List Form
                    , selected: Bool
                    , pos: (Int, Int)
                    , exp: Fragment
                    , forms: List Form}



type Action = DAction DragAction | BAction BlockAction

type BlockAction = Add BlockTemplate | None

type DragAction = Lift | MoveBy (Int, Int, Int) | Release Int

type alias Position = (Int, Int)

type alias ID = Int

type alias BlockTemplate = ID -> Block

