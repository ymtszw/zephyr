module Json.DecodeExtra exposing (conditional, leakyList, maybeField, succeedIf, tag, tagged, tagged2, tagged3, url, when)

import Json.Decode exposing (..)
import Url


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


{-| Merger of Json.Decode.field AND Json.Decode.maybe.

In some case, we want to decode a field into Maybe,
wherein the field may be absent OR always exist but can be null.

It practically just flattens Maybe value obtained from
`maybe (field "fieldName" (maybe decoder))`.

Also useful when introducing new field in serialized JSON format.

-}
maybeField : String -> Decoder a -> Decoder (Maybe a)
maybeField fieldName decoder =
    let
        flatten aMaybeMaybe =
            case aMaybeMaybe of
                Just (Just a) ->
                    Just a

                _ ->
                    Nothing
    in
    map flatten (maybe (field fieldName (maybe decoder)))


{-| Decode a serialized custom type value of 0-variable.

Use with EncodeExtra.tag.

-}
tag : String -> a -> Decoder a
tag constructorName constructor =
    when (field "tag" string) ((==) constructorName) (succeed constructor)


{-| Decode a serialized custom type value of 1-variable.

Use with EncodeExtra.tagged.

-}
tagged : String -> (a -> b) -> Decoder a -> Decoder b
tagged constructorName constructor value1Decoder =
    when (field "tag" string) ((==) constructorName) <|
        map constructor (field "v1" value1Decoder)


{-| Decode a serialized custom type value of 2-variable.

Use with EncodeExtra.tagged2.

-}
tagged2 : String -> (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
tagged2 constructorName constructor value1Decoder value2Decoder =
    when (field "tag" string) ((==) constructorName) <|
        map2 constructor
            (field "v1" value1Decoder)
            (field "v2" value2Decoder)


{-| Decode a serialized custom type value of 3-variable.

Use with EncodeExtra.tagged3.

-}
tagged3 : String -> (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
tagged3 constructorName constructor value1Decoder value2Decoder value3Decoder =
    when (field "tag" string) ((==) constructorName) <|
        map3 constructor
            (field "v1" value1Decoder)
            (field "v2" value2Decoder)
            (field "v3" value3Decoder)


{-| Decode a string into Url.Url.
-}
url : Decoder Url.Url
url =
    string
        |> andThen
            (\urlString ->
                case Url.fromString urlString of
                    Just decodedUrl ->
                        succeed decodedUrl

                    Nothing ->
                        fail ("URL is serialized incorrectly: " ++ urlString)
            )