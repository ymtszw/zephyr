module Json.EncodeExtra exposing (maybe)

import Json.Encode as E exposing (Value)


maybe : (a -> Value) -> Maybe a -> Value
maybe encoder valueMaybe =
    valueMaybe |> Maybe.map encoder |> Maybe.withDefault E.null
