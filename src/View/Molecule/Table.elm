module View.Molecule.Table exposing (Props, layoutFixed, render, styles)

import Html exposing (Attribute, Html, table, tbody, td, th, thead, tr)
import Html.Attributes exposing (class, colspan)
import Html.Keyed
import View.Atom.Background as Background
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (bold, colorNote, t)
import View.Style exposing (..)


type alias Props a msg =
    { columns : List { header : String, cell : a -> ( List (Attribute msg), List (Html msg) ) }
    , rowKey : a -> String
    , data : List a
    }


{-| Renders a simple table.

Uses `Html.Keyed.node` internally.
If `props.data` is empty, a row with "empty" message is inserted.

-}
render : List (Attribute msg) -> Props a msg -> Html msg
render attrs props =
    let
        headerCell c =
            th [ Background.colorNote ] [ t c.header ]

        rowKey d =
            ( props.rowKey d
            , tr [] (List.map (\c -> td_ (c.cell d)) props.columns)
            )

        td_ ( tdAttrs, kids ) =
            td tdAttrs kids
    in
    table (widthFill :: attrs)
        [ thead [] [ tr [ bold ] (List.map headerCell props.columns) ]
        , Html.Keyed.node "tbody" [] <|
            case props.data of
                [] ->
                    [ ( "emptyTable", tr [] [ td [ class emptyClass, colorNote, colspan 1000 ] [ t "(Empty)" ] ] ) ]

                nonEmptyData ->
                    List.map rowKey nonEmptyData
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
    , s (c emptyClass) [ ( "text-align", "center" ) ]
    ]


defaultBorderSpacing : Int
defaultBorderSpacing =
    3


layoutFixedClass : String
layoutFixedClass =
    "tblf"


emptyClass : String
emptyClass =
    "tbemp"
