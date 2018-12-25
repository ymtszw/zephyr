module Data.Color exposing
    ( Color, encode, decoder, hexDecoder
    , setAlpha, brightness, cssRgba, fromRgba, toRgba, fromHex, fromHexUnsafe, toHex
    )

{-| Color type and functions. Mimicks Element.Color in elm-ui.

@docs Color, encode, decoder, hexDecoder
@docs setAlpha, brightness, cssRgba, fromRgba, toRgba, fromHex, fromHexUnsafe, toHex

-}

import Hex
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Color
    = Color ColorRecord


type alias ColorRecord =
    { red : CFloat
    , green : CFloat
    , blue : CFloat
    , alpha : CFloat
    }


type CFloat
    = CFloat Float


encode : Color -> E.Value
encode c =
    let
        { red, green, blue, alpha } =
            toRgba c
    in
    E.object
        [ ( "red", E.float red )
        , ( "green", E.float green )
        , ( "blue", E.float blue )
        , ( "alpha", E.float alpha )
        ]


decoder : Decoder Color
decoder =
    D.map Color <|
        D.map4 ColorRecord
            (D.field "red" (D.map cap D.float))
            (D.field "green" (D.map cap D.float))
            (D.field "blue" (D.map cap D.float))
            (D.field "alpha" (D.map cap D.float))


{-| Decode a 6-digit Hexadecimal Color string into Element.Color.
-}
hexDecoder : String -> Decoder Color
hexDecoder hexStr =
    case fromHex hexStr of
        Ok c ->
            D.succeed c

        Err e ->
            D.fail <| "Invalid Color: '" ++ hexStr ++ "'. " ++ e


cap : Float -> CFloat
cap f =
    if f < 0 then
        CFloat 0

    else if f > 1 then
        CFloat 1

    else
        CFloat f


unwrap : CFloat -> Float
unwrap (CFloat f) =
    f


map : (Float -> Float) -> CFloat -> CFloat
map mapper (CFloat f) =
    cap (mapper f)


toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba (Color { red, green, blue, alpha }) =
    { red = unwrap red, green = unwrap green, blue = unwrap blue, alpha = unwrap alpha }


fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba { red, green, blue, alpha } =
    Color (ColorRecord (cap red) (cap green) (cap blue) (cap alpha))


black : Color
black =
    Color (ColorRecord (CFloat 0) (CFloat 0) (CFloat 0) (CFloat 1))


{-| Shift brightness of a Color (RGB) by a power of 1.15, without altering alpha.

`brightness 1` on a Color of `{red = 100, green = 100, blue = 100}`
will yield `{red = 115, green = 115, blue = 115}`.

`brightness -1` on the same Color will yield `{red = 86.96, green = 86.96, blue = 86.96}`

-}
brightness : Float -> Color -> Color
brightness power (Color cr) =
    let
        scale f =
            f * 1.15 ^ power
    in
    Color { cr | red = map scale cr.red, green = map scale cr.green, blue = map scale cr.blue }


setAlpha : Float -> Color -> Color
setAlpha a (Color cr) =
    Color { cr | alpha = cap a }


{-| Dump a Color to CSS-compatible representaiton
-}
cssRgba : Color -> String
cssRgba (Color cr) =
    String.join ""
        [ "rgba("
        , String.fromFloat (255 * unwrap cr.red)
        , ","
        , String.fromFloat (255 * unwrap cr.green)
        , ","
        , String.fromFloat (255 * unwrap cr.blue)
        , ","
        , String.fromFloat (unwrap cr.alpha)
        , ")"
        ]


fromHex : String -> Result String Color
fromHex hexStr_ =
    let
        hexStr =
            if String.startsWith "#" hexStr_ then
                String.dropLeft 1 hexStr_

            else
                hexStr_

        toCFloat int =
            cap (toFloat int / 255)
    in
    Result.map3 (\r g b -> Color (ColorRecord r g b (cap 1)))
        (hexStr |> String.slice 0 2 |> Hex.fromString |> Result.map toCFloat)
        (hexStr |> String.slice 2 4 |> Hex.fromString |> Result.map toCFloat)
        (hexStr |> String.slice 4 6 |> Hex.fromString |> Result.map toCFloat)


fromHexUnsafe : String -> Color
fromHexUnsafe surelyHexStr =
    Result.withDefault black (fromHex surelyHexStr)


toHex : Color -> String
toHex (Color cr) =
    let
        toHex2 =
            unwrap >> (*) 255 >> round >> Hex.toString >> String.padLeft 2 '0'
    in
    toHex2 cr.red ++ toHex2 cr.green ++ toHex2 cr.blue
