module Eval where

import Dict
import Model exposing (..)

evalStep : Model -> Model
evalStep m = m
--    let 
--        processBlock block exp = {block | exp <- evalExp exp}
--        updateModel block exp = {m | blocks <- Dict.insert 1 (processBlock block exp) m.blocks}
--    in
--        case Dict.get 1 m.blocks of
--            Just block -> 
--                case block.exp of
--                    E blockExp -> updateModel block blockExp
--                    _ -> m
--            _ -> m


--evalExp : Exp -> Exp
--evalExp exp = 
--    case exp of 
--        R rocks -> R rocks
--        C hof (R rocks) -> step hof rocks
--        C hof nestExp -> C hof (evalExp nestExp)


----mapStep : Func -> Rocks -> Rocks -> Exp
----mapStep func processedRocks unprocessedRocks = 


--step : HOF -> Rocks -> Exp
--step hof rocks =
--    case hof of 
--        Map Nothing _ -> rocks
--        Filter Nothing _ -> rocks
--        Map (Just func) processedRocks -> rocks 
--    -- unpack expression
--    -- find redex
--    -- change it and return