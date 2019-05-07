module View.Atoms.Button exposing
    ( link
    , styles
    )

{-| Button Atoms.

Color of buttons are decided by upstream themes and `Background`/`Typography` APIs.

@docs link
@docs styles

-}

import Color exposing (Color, toCssString)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import View.Atoms.Layout as Layout
import View.Atoms.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Atoms.Typography as Typography
import View.Style exposing (..)


{-| A.k.a "LinkButton".

Note that link buttons cannot be "disabled" like we do for standard buttons.

-}
link : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
link attrs opts =
    Typography.link (Attributes.class linkButtonClass :: attrs) opts


styles : List Style
styles =
    [ oneDarkDefaultFaceStyle
    , defaultFaceStyle aubergineClass aubergineTheme.bd
    ]
        ++ standardStyles
        ++ linkButtonStyles


standardStyles : List Style
standardStyles =
    [ s "button"
        [ ( "border-radius", "0.2em" )
        , ( "border-width", "0px" )
        , ( "cursor", "pointer" ) -- I deliberately want this. I am AGAINST the philosophy of "buttons do not need pointer."
        ]
        |> inject Layout.paddingInlineStyle
        |> inject oneDarkDefaultFaceStyle
    , s (hov "button") [ ( "opacity", "0.9" ) ]
    , s "button:disabled" [ ( "opacity", "0.7" ), ( "cursor", "default" ) ]
    ]


oneDarkDefaultFaceStyle : Style
oneDarkDefaultFaceStyle =
    -- .bd colors are used as defaults; these colors are not available as neither font nor BG colors
    defaultFaceStyle oneDarkClass oneDarkTheme.bd


defaultFaceStyle : String -> Color -> Style
defaultFaceStyle themeClass defaultColor =
    scoped (c themeClass) "button" [ ( "background-color", toCssString defaultColor ) ]


linkButtonStyles : List Style
linkButtonStyles =
    let
        linkSelectors =
            String.join ","
                [ "a" ++ c linkButtonClass ++ ":link"
                , "a" ++ c linkButtonClass ++ ":visited"
                , "a" ++ c linkButtonClass ++ ":hover"
                , "a" ++ c linkButtonClass ++ ":active"
                ]
    in
    -- Inline OR Block element that behave like a button but actually a link
    [ s (c linkButtonClass)
        [ ( "border-radius", "0.2em" )
        , ( "border-width", "0px" )
        , ( "cursor", "pointer" ) -- I deliberately want this. I am AGAINST the philosophy of "buttons do not need pointer."
        , ( "width", "fit-content" ) -- XXX Applied in order to fit width to contents regardless of `display` value or contents, but this might not be supported by all browsers
        ]
        |> inject Layout.paddingInlineStyle
        |> inject oneDarkDefaultFaceStyle
    , s (hov (c linkButtonClass)) [ ( "opacity", "0.9" ) ]
    , s linkSelectors [ ( "color", "inherit" ), ( "text-decoration", "none" ) ] -- Cancelling default link decorations
    ]


linkButtonClass : String
linkButtonClass =
    "btn"
