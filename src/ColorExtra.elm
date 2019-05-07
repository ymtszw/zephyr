module ColorExtra exposing
    ( encode, decoder
    , setAlpha, brightness, fromHex, fromHexUnsafe, toHex
    )

{-| Additional Color functions for elm-color.

@docs encode, decoder
@docs setAlpha, brightness, fromHex, fromHexUnsafe, toHex

-}

import Color exposing (Color)
import Hex
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E


encode : Color -> E.Value
encode c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    E.object
        [ ( "red", E.float red )
        , ( "green", E.float green )
        , ( "blue", E.float blue )
        , ( "alpha", E.float alpha )
        ]


decoder : Decoder Color
decoder =
    D.oneOf
        [ -- Well-structured; for internal storage
          D.map4 Color.rgba
            (D.field "red" (D.map cap D.float))
            (D.field "green" (D.map cap D.float))
            (D.field "blue" (D.map cap D.float))
            (D.field "alpha" (D.map cap D.float))
        , -- From Hexadecimal color string
          D.do D.string <|
            \hexStr ->
                case fromHex hexStr of
                    Ok c ->
                        D.succeed c

                    Err e ->
                        D.fail <| "Invalid Color: '" ++ hexStr ++ "'. " ++ e
        ]


{-| Currently elm-color does not cap overflown numbers. This is a shim for that.
-}
cap : Float -> Float
cap f =
    if f < 0 then
        0

    else if f > 1 then
        1

    else
        f


{-| This function exists in elm-color (with more feature and slightly different signature), but currently unexposed.
-}
fromHex : String -> Result String Color
fromHex hexStr_ =
    let
        hexStr =
            if String.startsWith "#" hexStr_ then
                String.dropLeft 1 hexStr_

            else
                hexStr_

        toCappedFloat int =
            cap (toFloat int / 255)
    in
    Result.map3 Color.rgb
        (hexStr |> String.slice 0 2 |> Hex.fromString |> Result.map toCappedFloat)
        (hexStr |> String.slice 2 4 |> Hex.fromString |> Result.map toCappedFloat)
        (hexStr |> String.slice 4 6 |> Hex.fromString |> Result.map toCappedFloat)


fromHexUnsafe : String -> Color
fromHexUnsafe surelyHexStr =
    Result.withDefault Color.black (fromHex surelyHexStr)


toHex : Color -> String
toHex c =
    let
        { red, green, blue } =
            Color.toRgba c

        toHex2 =
            cap >> (*) 255 >> round >> Hex.toString >> String.padLeft 2 '0'
    in
    toHex2 red ++ toHex2 green ++ toHex2 blue



-- MANIPULATION


{-| Shift brightness of a Color (RGB) by a power of 1.15, without altering alpha.

`brightness 1` on a Color of `{red = 100, green = 100, blue = 100}`
will yield `{red = 115, green = 115, blue = 115}`.

`brightness -1` on the same Color will yield `{red = 86.96, green = 86.96, blue = 86.96}`

-}
brightness : Float -> Color -> Color
brightness power c =
    let
        cr =
            Color.toRgba c

        scale f =
            f * 1.15 ^ power
    in
    Color.fromRgba
        { cr
            | red = cappedMap scale cr.red
            , green = cappedMap scale cr.green
            , blue = cappedMap scale cr.blue
        }


cappedMap : (Float -> Float) -> Float -> Float
cappedMap mapper f =
    cap (mapper f)


setAlpha : Float -> Color -> Color
setAlpha a c =
    let
        cr =
            Color.toRgba c
    in
    Color.fromRgba { cr | alpha = cap a }
