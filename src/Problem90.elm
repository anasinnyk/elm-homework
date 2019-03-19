module Problem90 exposing (..)

import List.Extra exposing (permutations, unique, zip)
import List exposing (map, filter, any, member)

queens : Int -> List (List (Int, Int))
queens d = 
    let
        side : List Int
        side = List.range 1 d

        sameDiag : Int -> List Int -> Bool
        sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) <| zip side qs
        
        isSafe : Int -> List Int -> Bool
        isSafe try qs = not (member try qs || sameDiag try qs)
        
        test : List Int -> Bool
        test l = case l of
            [] -> True
            (q::qs) -> isSafe q qs && test qs
    in
        map (zip side) <| filter test <| permutations side
