module Data.Producer.Slack.Ts exposing (Ts, decoder, encode, toPosix, toString)

import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)


{-| Slack messages' ID AND timestamps.

This is unique per channel, sorted, and can be translated as Posix timestamps.

For convenience, they are parsed as Time.Posix at decoding and results are attached in the type.

-}
type Ts
    = Ts String Posix


encode : Ts -> E.Value
encode (Ts ts posix) =
    E.tagged2 "Ts" (E.string ts) (E.int (Time.posixToMillis posix))


decoder : Decoder Ts
decoder =
    D.oneOf
        [ D.tagged2 "Ts" Ts D.string (D.map Time.millisToPosix D.int)
        , -- From APIs
          D.do D.string <|
            \tsStr ->
                case String.toFloat tsStr of
                    Just seconds ->
                        -- ts values are only valid as timestamps to seconds. Decimal values are "uniqifiers", not sub-second values
                        D.succeed (Ts tsStr (Time.millisToPosix (floor seconds * 1000)))

                    Nothing ->
                        D.fail "Invalid `ts` value"
        ]


toString : Ts -> String
toString (Ts str _) =
    str


toPosix : Ts -> Posix
toPosix (Ts _ posix) =
    posix
