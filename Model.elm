module Model where

import Color exposing (Color)
import Maybe exposing (Maybe)
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (Form)

type alias Model = {nextID: ID, blocks: List (Exp)}

type Exp =  H HOF 
            | L RockList 
            | F Func
type HOF =  Filter Block (Maybe Func) (Maybe RockList) 
            | Map Block (Maybe Func) (Maybe RockList)

type Func = P Pred | T Transform

type alias Pred = {name: String
            , func: (Rock -> Bool)
            , block: Block}

type alias Transform = {name: String
                , func: (Rock -> Rock )
                , block: Block}

type alias Block = {id: ID, form: Form, selected: Bool, pos: (Int, Int)}

type alias RockList = List Rock

type alias Rock = {value: Int
            , solid: Bool
            , color: Color}

type Action = DAction DragAction | BAction BlockAction

type BlockAction = Add Block | None

type DragAction = Lift | MoveBy (Int, Int, Int) | Release

type alias ID = Int






