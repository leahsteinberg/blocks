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


type HOF = Filter (Maybe (Fun Func)) (Maybe (RV Rocks)) | Map (Maybe (Fun Func)) (Maybe (RV Rocks))

type Value = Rocks | Rock

--type Exp =  H HOF 
--            | RE RockExpression
--            | F Func

--type RockExpression = Higher HOF | R Rocks

--type HOF =  Filter (Maybe Func) (Maybe RockExpression) 
--            | Map (Maybe Func) (Maybe RockExpression)

type Func = P Pred | T Transform | A Accum

type Pred = (Rock -> Bool)

type Transform = (Rock -> Rock)

type Accum = (Rock -> a -> a)


--type CornerAction = ((Int, Int), (Int, Int))
--type RockFunc =  | (Rock -> Rock)

--type alias FuncInfo = {name: String
--            , func: RockFunc}

--type alias Transform = {name: String
--                , func: RockFunc}

type alias Block = {id: ID
                    , ele: List Form
                    , selected: Bool
                    , pos: (Int, Int)
                    , exp: Exp
                    , forms: List Form}





type Action = DAction DragAction | BAction BlockAction

type BlockAction = Add BlockTemplate | None

type DragAction = Lift | MoveBy (Int, Int, Int) | Release Int

type alias Position = (Int, Int)

type alias ID = Int

type alias BlockTemplate = ID -> Block






