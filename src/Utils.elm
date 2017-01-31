module Utils exposing (..)

intOr : Int -> String -> Int
intOr dflt input =
    case String.toInt input of
        Ok val -> val
        Err _ -> dflt
