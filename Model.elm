module Model where


import Color exposing (Color)
import Maybe exposing (Maybe)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (Form)
import Dict exposing (Dict)

type alias Model = {nextID: ID, blocks: Dict ID Block}

type Exp =  H HOF 
            | R Rocks 
            | F Func

type RockExpression = HOF | Rocks

type HOF =  Filter (Maybe Func) (Maybe RockExpression) 
            | Map (Maybe Func) (Maybe RockExpression)

type Func = P Pred | T Transform

type alias Pred = {name: String
            , func: (Rock -> Bool)}

type alias Transform = {name: String
                , func: (Rock -> Rock)}

type alias Block = {id: ID, ele: Element, selected: Bool, pos: (Int, Int), exp: Exp, forms: List Form}

type alias Rocks = List Rock

type alias Rock = {value: Int
            , solid: Bool
            , color: Color}

type Action = DAction DragAction | BAction BlockAction

type BlockAction = Add BlockTemplate | None

type DragAction = Lift | MoveBy (Int, Int, Int) | Release

type alias Position = (Int, Int)

type alias ID = Int

type alias BlockTemplate = ID -> Block






