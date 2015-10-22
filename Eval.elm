module Eval where
import Debug
import Dict
import Model exposing (..)
import ViewFragment exposing (fragmentToForms)

evalStep : Model -> Model
evalStep m = List.foldl evalBlock m (Dict.values m.blocks) 

evalBlock : Block -> Model -> Model
evalBlock block m =
    let 
        
        updateModel block exp = 
            let 
                newFragment = E (evalExp exp) 
                (ele, forms) = fragmentToForms newFragment block.id
                processBlock block exp = {block | exp <- newFragment
                                        , ele <- ele
                                        , forms <- forms}
            in
                {m | blocks <- Dict.insert block.id (processBlock block exp) m.blocks}
    in
        case block.exp of
            E blockExp -> updateModel block blockExp
            _ -> m





evalExp : Exp -> Exp
evalExp exp = 
    case exp of 
        R rocks -> R rocks
        C hof (R rocks) -> step hof rocks
        C hof nestExp -> C hof (evalExp nestExp)


step : HOF -> Rocks -> Exp
step hof rocks =   
    case (hof, rocks) of 
        --(Map Nothing _, _) -> R rocks
        --(Filter Nothing _, _) -> R rocks
        (Filter _ processedRocks, []) -> R processedRocks

        (Map _ processedRocks, []) -> R processedRocks

        (Map (Just (T transform)) processedRocks, hd::tl) -> 
            C (Map (Just (T transform)) (processedRocks ++ [(transform hd)])) (R tl)

        (Filter (Just (P pred)) processedRocks, hd::tl) -> 
            let updatedRocks = if (Debug.watch "pred" (pred hd)) then processedRocks ++ [hd] else processedRocks in
            C (Filter (Just (P pred)) (updatedRocks)) (R tl)

        (Fold (Just (A accum accRock)) processedRocks, hd::tl) -> 
            let newAccumRock = accum hd accRock in
            C (Fold (Just (A accum newAccumRock) )[]) (R tl)
        (Fold (Just (A accum accRock)) processedRocks, []) -> R [accRock]
            
        _ -> R rocks
    -- unpack expression
    -- find redex
    -- change it and return



