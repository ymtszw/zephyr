module View.Style exposing
    ( Style, toString, s, kf, pure, derive, inject, scoped
    , c, hov, descOf, px, scaleByQuarter, scale12, none, noAttr
    )

{-| Style entry type and its manipulations.

Provides necessary types and helper functions.

@docs Style, toString, s, kf, pure, derive, inject, scoped
@docs c, hov, descOf, px, scaleByQuarter, scale12, none, noAttr

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes
import Json.Encode


{-| An entry in stylesheet.

Currently only supports raw style entries and `@keyframes`.
But it is possible to also support `@media` queries.

-}
type Style
    = RawStyle Raw
    | KeyFrames KF


type KF
    = KF String (List Raw)


type Raw
    = Raw String (Dict String String)


{-| Produces a `RawStyle` entry for a selector.

    toString (s "p" [ ( "color", "red" ) ])
    --> "p{color:red;}"

-}
s : String -> List ( String, String ) -> Style
s selector props =
    RawStyle (Raw selector (Dict.fromList props))


{-| Creates an empty `RawStyle` entry for a selector.

You will use this with `inject`.

-}
pure : String -> Style
pure selector =
    RawStyle (Raw selector Dict.empty)


{-| Produces a `@keyframes` entry.

    toString <| kf "rotating"
        [ ( "from", [ ("transform", "rotate(0turn)" ) ] )
        , ( "to", [ ("transform", "rotate(1turn)" ) ] )
        ]
    --> "@keyframes rotating{from{transform:rotate(0turn);}to{transform:rotate(1turn);}}"

-}
kf : String -> List ( String, List ( String, String ) ) -> Style
kf animationName frameBlocks =
    KeyFrames <|
        KF animationName <|
            List.map (\( frameSelector, props ) -> Raw frameSelector (Dict.fromList props)) frameBlocks


{-| Derives a new Style from another, by copying its properties.
-}
derive : String -> Style -> Style
derive selector base =
    inject base (pure selector)


{-| Injects properties in a `Style` (first one) into another (second) one.

Can be used for creating a `Style` value by combining existing `Style`s together.

The second `Style` is considered a "base", and the first one is a "mixin".
If properties collide, ones in the first `Style` ("new" one) take effect.

You can only combine `RawStyle`s currently.
"Base" `KeyFrame`s are kept intact and "mixin" `KeyFrame`s are just ignored.

    pure ".someClass"
        |> inject Typography.sansSerif
        |> inject Typography.bold
        |> inject myLuckyColor
        |> toString
    --> ".someClass{font-family:Tahoma,Verdana,Arial,Helvetica,sans-serif;font-weight:700;color:#00cc99;}"

-}
inject : Style -> Style -> Style
inject new base =
    case base of
        RawStyle (Raw selector baseProps) ->
            case new of
                KeyFrames _ ->
                    base

                RawStyle (Raw _ newProps) ->
                    RawStyle (Raw selector (Dict.union newProps baseProps))

        KeyFrames _ ->
            base


{-| Generates a scoped style with scope-defining selector and target selector.

One of them can be a tag, but NOT both!
Since it tries to create "inclusive" scope, where the scope-defining selector
and the target selector can match on a single element.
An element cannot be two tags!

-}
scoped : String -> String -> List ( String, String ) -> Style
scoped scopeSelector targetSelector props =
    let
        selector =
            String.join ","
                [ -- Same element
                  if String.startsWith "." targetSelector then
                    scopeSelector ++ targetSelector

                  else
                    -- targetSelector is a tag, must come first
                    targetSelector ++ scopeSelector
                , descOf scopeSelector targetSelector -- Descendants
                ]
    in
    s selector props


{-| Renders a `Style` into bare string.
-}
toString : Style -> String
toString style =
    case style of
        RawStyle raw ->
            rawToString raw

        KeyFrames (KF animationName frameBlocks) ->
            String.join ""
                [ "@keyframes "
                , animationName
                , "{"
                , String.join "" (List.map rawToString frameBlocks)
                , "}"
                ]


rawToString : Raw -> String
rawToString (Raw selector props) =
    String.join ""
        [ selector
        , "{"
        , Dict.foldl (\name value acc -> acc ++ name ++ ":" ++ value ++ ";") "" props
        , "}"
        ]



-- HELPERS


c : String -> String
c className =
    "." ++ className


{-| Generates a style for `:hover` pseudo-class of a selector.
-}
hov : String -> String
hov selector =
    selector ++ ":hover"


descOf : String -> String -> String
descOf ancestor descendant =
    ancestor ++ " " ++ descendant


px : Int -> String
px i =
    String.fromInt i ++ "px"


scale12 : Int -> Int
scale12 =
    scaleByQuarter 12


scaleByQuarter : Int -> Int -> Int
scaleByQuarter base factor =
    if factor == 0 then
        base

    else
        floor (toFloat base * 1.25 ^ toFloat factor)


none : Html msg
none =
    Html.text ""


noAttr : Attribute msg
noAttr =
    Html.Attributes.property "none" Json.Encode.null
