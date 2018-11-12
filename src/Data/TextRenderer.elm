module Data.TextRenderer exposing
    ( TextRenderer
    , default
    , StringOrUrl(..), parseIntoStringOrUrlList
    )

{-| Render user-generated texts into Html.

In order to use other rendering libraries,
this module generates plain old Html Msg.
They must be converted to Element Msg by callers.


## Types

@docs TextRenderer


## APIs

@docs default


## Exposed for tests

@docs StringOrUrl, parseIntoStringOrUrlList

-}

import Data.ColorTheme exposing (ColorTheme)
import Data.Msg exposing (Msg)
import Html exposing (..)
import Html.Attributes exposing (href, rel, style, target)
import Parser exposing ((|.), (|=), Parser, Step(..))
import Url exposing (Url)
import View.Parts exposing (css)


type alias TextRenderer =
    String -> List (Html Msg)


{-| Default text renderer.

  - Detects http/https URLs and turn them into link element
      - URLs must be surrounded by whitespaces

-}
default : ColorTheme -> TextRenderer
default theme message =
    case Parser.run parseIntoStringOrUrlList message of
        Ok stringOrUrlList ->
            List.map (stringOrUrlEl theme) stringOrUrlList

        Err _ ->
            [ stringOrUrlEl theme (S message) ]


stringOrUrlEl : ColorTheme -> StringOrUrl -> Html Msg
stringOrUrlEl theme stringOrUrl =
    case stringOrUrl of
        S string ->
            text string

        U url ->
            let
                shortUrl =
                    url.host ++ url.path

                trimmedUrl =
                    if String.endsWith "/" shortUrl then
                        String.dropRight 1 shortUrl

                    else
                        shortUrl
            in
            a
                [ href (Url.toString url)
                , target "_blank"
                , rel "noreferrer noopener"
                , style "color" (css theme.link)
                ]
                [ text trimmedUrl ]



-- PARSER


type StringOrUrl
    = S String
    | U Url


parseIntoStringOrUrlList : Parser (List StringOrUrl)
parseIntoStringOrUrlList =
    Parser.loop [] <|
        \acc ->
            Parser.oneOf
                [ Parser.end |> Parser.map (\_ -> Done (List.foldl compactify [] acc))
                , chompUrlLike |> Parser.map (\strOrUrl -> Loop (strOrUrl :: acc))
                , chompNonUrl |> Parser.map (\str -> Loop (S str :: acc))
                ]


compactify : StringOrUrl -> List StringOrUrl -> List StringOrUrl
compactify elem acc =
    case acc of
        (S x) :: xs ->
            case elem of
                S y ->
                    S (y ++ x) :: xs

                U _ ->
                    elem :: acc

        _ ->
            elem :: acc


chompUrlLike : Parser StringOrUrl
chompUrlLike =
    let
        parseAsUrl nonSpaceStr =
            case Url.fromString ("http" ++ nonSpaceStr) of
                Just url ->
                    U url

                Nothing ->
                    S ("http" ++ nonSpaceStr)
    in
    Parser.succeed parseAsUrl
        |. Parser.token "http"
        |= Parser.getChompedString (Parser.chompWhile notSpaces)


chompNonUrl : Parser String
chompNonUrl =
    Parser.getChompedString <| Parser.chompUntilEndOr "http"


notSpaces : Char -> Bool
notSpaces c =
    not <| List.member c [ ' ', '\t', '\n', '\u{000D}' ]
