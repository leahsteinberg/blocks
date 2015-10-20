module Eval where

import Dict
import Model exposing (..)
import ViewFragment exposing (fragmentToForms)

evalStep : Model -> Model
evalStep m = 
    let 
        
        updateModel block exp = 
            let 
                newFragment = E (evalExp exp) 
                (ele, forms) = fragmentToForms newFragment 1
                processBlock block exp = {block | exp <- newFragment
                                        , ele <- ele
                                        , forms <- forms}
            in
                {m | blocks <- Dict.insert 1 (processBlock block exp) m.blocks}
    in
        case Dict.get 1 m.blocks of
            Just block -> 
                case block.exp of
                    E blockExp -> updateModel block blockExp
                    _ -> m
            _ -> m


evalExp : Exp -> Exp
evalExp exp = 
    case exp of 
        R rocks -> R rocks
        C hof (R rocks) -> step hof rocks
        C hof nestExp -> C hof (evalExp nestExp)


mapStep : Func -> Rocks -> Rock -> Rocks
mapStep func processedRocks rock = 
    case func of 
        T transform -> (transform rock)::processedRocks
        _ -> rock::processedRocks


step : HOF -> Rocks -> Exp
step hof rocks =   
    case (hof, rocks) of 
        (Map Nothing _, _) -> R rocks
        (Filter Nothing _, _) -> R rocks
        (Filter _ processedRocks, []) -> R processedRocks
        (Map _ processedRocks, []) -> R processedRocks
        (Map (Just func) processedRocks, hd::tl) -> 
            C (Map (Just func) (mapStep func processedRocks hd)) (R tl)
    -- unpack expression
    -- find redex
    -- change it and return