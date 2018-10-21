module Json.EncodeExtra exposing (maybe, tag, tagged, tagged2, tagged3, url)

import Json.Encode as E exposing (Value)
import Url


maybe : (a -> Value) -> Maybe a -> Value
maybe encoder valueMaybe =
    valueMaybe |> Maybe.map encoder |> Maybe.withDefault E.null


{-| Serialize 0-variable custom type value into `{"tag":"ConstructorName"}`

Use with DecodeExtra.tag.

-}
tag : String -> Value
tag constructorName =
    E.object [ ( "tag", E.string constructorName ) ]


{-| Serialize 1-variable custom type value into
`{"tag":"ConstructorName","v1":value1}`

Use with DecodeExtra.tagged.

-}
tagged : String -> Value -> Value
tagged constructorName value1 =
    E.object
        [ ( "tag", E.string constructorName )
        , ( "v1", value1 )
        ]


{-| Serialize 2-variable custom type value into
`{"tag":"ConstructorName","v1":value1,"v2":value2}`
-}
tagged2 : String -> Value -> Value -> Value
tagged2 constructorName value1 value2 =
    E.object
        [ ( "tag", E.string constructorName )
        , ( "v1", value1 )
        , ( "v2", value2 )
        ]


{-| Serialize 3-variable custom type value into
`{"tag":"ConstructorName","v1":value1,"v2":value2,"v3":value3}`
-}
tagged3 : String -> Value -> Value -> Value -> Value
tagged3 constructorName value1 value2 value3 =
    E.object
        [ ( "tag", E.string constructorName )
        , ( "v1", value1 )
        , ( "v2", value2 )
        , ( "v3", value3 )
        ]


url : Url.Url -> Value
url u =
    E.string (Url.toString u)
