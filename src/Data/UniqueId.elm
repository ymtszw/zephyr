module Data.UniqueId exposing (Generator, gen, init)

{-| Generates Unique (sequenced) ID string.
-}

import Dict exposing (Dict)


type Generator
    = Generator (Dict String Int)


init : Generator
init =
    Generator Dict.empty


gen : String -> Generator -> ( String, Generator )
gen prefix (Generator dict) =
    case Dict.get prefix dict of
        Just prev ->
            genImpl prefix (prev + 1) (Generator dict)

        Nothing ->
            genImpl prefix 0 (Generator dict)


genImpl : String -> Int -> Generator -> ( String, Generator )
genImpl prefix current (Generator dict) =
    ( genIdString prefix current
    , Generator (Dict.insert prefix current dict)
    )


genIdString : String -> Int -> String
genIdString prefix current =
    prefix ++ "_" ++ String.fromInt current
