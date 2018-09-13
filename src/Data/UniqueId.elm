module Data.UniqueId exposing (Generator, encodeGenerator, gen, genAndMap, generatorDecoder, init)

{-| Generates Unique (sequenced) ID string.
-}

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Generator
    = Generator (Dict String Int)


init : Generator
init =
    Generator Dict.empty


encodeGenerator : Generator -> E.Value
encodeGenerator (Generator dict) =
    E.dict identity E.int dict


generatorDecoder : Decoder Generator
generatorDecoder =
    D.map Generator (D.dict D.int)


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


{-| Generate an ID, then use it for a function.

Used in everyday cases where you need your function result AND altered Generator.

Has unusual argument order in that transform function not coming last.
This is to allow writing in this style:

    genAndMap "prefix" generator <|
        \id ->
            actualFunctionYouWantToDo id

-}
genAndMap : String -> Generator -> (String -> a) -> ( a, Generator )
genAndMap prefix generator transform =
    Tuple.mapFirst transform (gen prefix generator)
