module View.Molecules.RawColumnItem exposing (render, styles)

import Data.Column as Column exposing (ColumnItem)
import Html exposing (Html, pre, text)
import Html.Attributes exposing (class)
import Json.Encode
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Layout exposing (padding5)
import View.Style exposing (..)


render : ColumnItem -> Html msg
render ci =
    pre [ class rawColumnItemClass, padding5, Background.colorBg, Border.round5 ]
        [ text (Json.Encode.encode 2 (Column.encodeColumnItem ci)) ]


styles : List Style
styles =
    [ s (c rawColumnItemClass)
        [ ( "max-width", px maxWidth )
        , ( "max-height", "80vh" )
        , ( "overflow", "auto" )
        ]
    ]


rawColumnItemClass : String
rawColumnItemClass =
    "rci"


maxWidth : Int
maxWidth =
    800
