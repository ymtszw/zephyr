module View.Organism.ColumnContainer exposing (Effects, Props, render, styles)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, draggable, id)
import Html.Events
import Html.Keyed
import Json.Decode exposing (succeed)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDark)
import View.Atom.Typography exposing (..)
import View.Organism.Sidebar exposing (sidebarWidth)
import View.Style exposing (..)


type alias Props c =
    { columns : List c
    }


type alias Effects msg =
    { columnDragEnd : msg
    }


render : Effects msg -> Props c -> Html msg
render eff p =
    Html.Keyed.node "div"
        [ class columnCtnrClass
        , id columnAreaParentId
        , flexRow
        , oneDark
        , Html.Events.on "dragend" (succeed eff.columnDragEnd)
        ]
        (List.indexedMap (columnWrapperKey eff) p.columns)


columnAreaParentId : String
columnAreaParentId =
    "columnAreaParent"


columnWrapperKey : Effects msg -> Int -> c -> ( String, Html msg )
columnWrapperKey eff index c =
    Tuple.pair ("column_" ++ String.fromInt index) <|
        div
            [ class columnWrapperClass
            , flexBasisAuto
            , Border.w1
            , Border.solid
            , Border.colorBg
            , Background.colorMain
            ]
            [ t (String.fromInt index)
            ]


styles : List Style
styles =
    [ s (c columnCtnrClass)
        [ ( "position", "fixed" )
        , ( "left", px sidebarWidth )
        , ( "top", "0" )
        , ( "height", "100vh" )
        , ( "max-width", "calc(100vw - " ++ px sidebarWidth ++ ")" )
        , ( "overflow-x", "scroll" )
        ]
    , s (c columnWrapperClass)
        [ ( "width", px columnWidth )
        ]
    ]


columnCtnrClass : String
columnCtnrClass =
    "cctnr"


columnWrapperClass : String
columnWrapperClass =
    "cwrap"


columnWidth : Int
columnWidth =
    350
