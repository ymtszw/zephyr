module View.Molecule.Table exposing (Props, layoutFixed, render, styles)

import Html exposing (Attribute, Html, table, tbody, td, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Keyed
import View.Atom.Background as Background
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (bold, t)
import View.Style exposing (..)


type alias Props a msg =
    { columns : List { header : String, cell : a -> Html msg }
    , rowKey : a -> String
    , data : List a
    }


render : List (Attribute msg) -> Props a msg -> Html msg
render attrs props =
    let
        headerCell c =
            th [ padding2, Background.colorNote ] [ t c.header ]

        rowKey d =
            ( props.rowKey d
            , tr [] (List.map (\c -> td [ padding2 ] [ c.cell d ]) props.columns)
            )
    in
    table (widthFill :: attrs)
        [ thead [] [ tr [ bold ] (List.map headerCell props.columns) ]
        , Html.Keyed.node "tbody" [] (List.map rowKey props.data)
        ]


layoutFixed : Attribute msg
layoutFixed =
    class layoutFixedClass



-- STYLES


styles : List Style
styles =
    [ s "table"
        [ ( "border-collapse", "separate" )
        , ( "border-spacing", px defaultBorderSpacing )
        ]
    , s "td,th" [ ( "overflow-x", "auto" ), ( "vertical-align", "top" ) ]
    , s (c layoutFixedClass) [ ( "table-layout", "fixed" ) ]
    ]


defaultBorderSpacing : Int
defaultBorderSpacing =
    2


layoutFixedClass : String
layoutFixedClass =
    "tblf"
