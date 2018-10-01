module Json.DecodeExtra exposing (conditional, leakyList, succeedIf, when)

import Json.Decode exposing (..)


{-| Apply first Decoder. If it succeeds, apply second one and yields its results.
If not, fail as a whole.

Semantically it is Applicative's `*>`.

-}
conditional : Decoder any -> Decoder actual -> Decoder actual
conditional condition actual =
    condition |> andThen (\_ -> actual)


{-| Apply a Decoder and check its yielding value.

If the check returned true, the whole Decoder succeeds with Unit (discarding the value).
Otherwise fail as a whole.

-}
succeedIf : Decoder a -> (a -> Bool) -> Decoder ()
succeedIf valueDecoder valueCheck =
    valueDecoder
        |> andThen
            (\value ->
                if valueCheck value then
                    succeed ()

                else
                    fail "Not passing value check."
            )


{-| Combining succeedIf and conditional.

Same af Json.Decode.Extra.when
<https://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra#when>

-}
when : Decoder a -> (a -> Bool) -> Decoder b -> Decoder b
when valueDecoder valueCheck actual =
    conditional (succeedIf valueDecoder valueCheck) actual


{-| Similar to Json.Decode.list, but it just ignores undecodable elements in the list instead of failing.

Consequently, it always succeeds with some list.
If all elements failed to be decoded, it succeeds with an empty list.

-}
leakyList : Decoder a -> Decoder (List a)
leakyList decoder =
    map (List.filterMap (decodeValue decoder >> Result.toMaybe)) (list value)
