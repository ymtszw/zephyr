module Data.Column.IdGenerator exposing (idGenerator)

{-| Generator of 24-chars random string for ID usages.
-}

import Random


idGenerator : Random.Generator String
idGenerator =
    Random.uniform zero nonzeroAlphanum
        |> Random.list length
        |> Random.map String.fromList


length : Int
length =
    24


zero : Char
zero =
    '0'


nonzeroAlphanum : List Char
nonzeroAlphanum =
    let
        uppers =
            List.range 65 90

        lowers =
            List.range 97 122

        oneToNine =
            List.range 49 57
    in
    List.map Char.fromCode (uppers ++ lowers ++ oneToNine)
