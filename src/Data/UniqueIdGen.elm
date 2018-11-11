module Data.UniqueIdGen exposing
    ( UniqueIdGen, encode, decoder, init
    , gen, genAndMap, andThen, sequence
    , columnPrefix, systemMessagePrefix, logEntryPrefix
    )

{-| Generates Unique (sequenced) ID string.

@docs UniqueIdGen, encode, decoder, init
@docs gen, genAndMap, andThen, sequence
@docs columnPrefix, systemMessagePrefix, logEntryPrefix

-}

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type UniqueIdGen
    = UniqueIdGen (Dict String Int)


init : UniqueIdGen
init =
    UniqueIdGen Dict.empty


encode : UniqueIdGen -> E.Value
encode (UniqueIdGen dict) =
    E.dict identity E.int dict


decoder : Decoder UniqueIdGen
decoder =
    D.map UniqueIdGen (D.dict D.int)


gen : String -> UniqueIdGen -> ( String, UniqueIdGen )
gen prefix (UniqueIdGen dict) =
    case Dict.get prefix dict of
        Just prev ->
            genImpl prefix (prev + 1) (UniqueIdGen dict)

        Nothing ->
            genImpl prefix 0 (UniqueIdGen dict)


genImpl : String -> Int -> UniqueIdGen -> ( String, UniqueIdGen )
genImpl prefix current (UniqueIdGen dict) =
    ( genIdString prefix current
    , UniqueIdGen (Dict.insert prefix current dict)
    )


genIdString : String -> Int -> String
genIdString prefix current =
    prefix ++ "_" ++ String.fromInt current


{-| Generate an ID, then use it for a function.

Used in everyday cases where you need your function result AND altered UniqueIdGen.

Has unusual argument order in that transform function not coming last.
This is to allow writing in this style:

    genAndMap "prefix" generator <|
        \id ->
            actualFunctionYouWantToDo id

-}
genAndMap : String -> UniqueIdGen -> (String -> a) -> ( a, UniqueIdGen )
genAndMap prefix generator transform =
    Tuple.mapFirst transform (gen prefix generator)


{-| -}
andThen : (( a, UniqueIdGen ) -> ( b, UniqueIdGen )) -> ( a, UniqueIdGen ) -> ( b, UniqueIdGen )
andThen nextGen prev =
    nextGen prev


{-| Sequentially generate IDs and apply them to list of functions.
-}
sequence : String -> UniqueIdGen -> List (String -> a) -> ( List a, UniqueIdGen )
sequence prefix generator transformSeq =
    let
        reducer transform ( accSeq, accGen ) =
            genAndMap prefix accGen <| \id -> transform id :: accSeq
    in
    List.foldr reducer ( [], generator ) transformSeq


columnPrefix : String
columnPrefix =
    "column"


systemMessagePrefix : String
systemMessagePrefix =
    "systemMessage"


logEntryPrefix : String
logEntryPrefix =
    "logEntry"
