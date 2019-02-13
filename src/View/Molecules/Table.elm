module View.Molecules.Table exposing (Props, alignMiddle, layoutFixed, render, styles)

import Html exposing (Attribute, Html, table, td, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Keyed
import View.Atoms.Background as Background
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (bold, colorNote, t)
import View.Style exposing (..)


type alias Props a msg =
    { columns :
        List
            { header : String
            , cell : a -> ( List (Attribute msg), List (Html msg) )
            }
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
            th [ padding2, Background.colorNote ] [ t c.header ]

        rowKey d =
            ( props.rowKey d
            , tr [] (List.map (\c -> td_ (c.cell d)) props.columns)
            )

        td_ ( tdAttrs, kids ) =
            td tdAttrs kids
    in
    table attrs
        [ thead [] [ tr [ bold ] (List.map headerCell props.columns) ]
        , Html.Keyed.node "tbody" [] <|
            case props.data of
                [] ->
                    [ ( "emptyTable", tr [] [ td [ class emptyClass, colorNote ] [ t "(Empty)" ] ] ) ]

                nonEmptyData ->
                    List.map rowKey nonEmptyData
        ]


layoutFixed : Attribute msg
layoutFixed =
    class layoutFixedClass


alignMiddle : Attribute msg
alignMiddle =
    class alignMiddleClass



-- STYLES


styles : List Style
styles =
    [ s "table"
        [ ( "border-collapse", "separate" )
        , ( "border-spacing", px defaultBorderSpacing )
        , -- Same trick used in flex-wrap spacing; compensating border-spacing
          ( "transform", "translate(" ++ px (negate defaultBorderSpacing) ++ "," ++ px (negate defaultBorderSpacing) ++ ")" )
        , ( "width", "calc(100% + " ++ px (defaultBorderSpacing * 2) ++ ")" )
        ]
    , s "td" [ ( "overflow-x", "auto" ), ( "vertical-align", "top" ) ]
    , s (c layoutFixedClass) [ ( "table-layout", "fixed" ) ]
    , s (c alignMiddleClass) [ ( "vertical-align", "middle" ) ]
    , s (c emptyClass) [ ( "text-align", "center" ) ]
    ]


defaultBorderSpacing : Int
defaultBorderSpacing =
    3


layoutFixedClass : String
layoutFixedClass =
    "tblf"


alignMiddleClass : String
alignMiddleClass =
    "tbvam"


emptyClass : String
emptyClass =
    "tbemp"
