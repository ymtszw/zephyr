module View.Organism.ColumnContainer exposing (Props, Effects, DragStatus(..), render, styles)

{-| Column Container Organism.

Handles column swapping.

@docs Props, Effects, DragStatus, render, styles

-}

import Color exposing (cssRgba)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, draggable, id)
import Html.Events exposing (on, preventDefaultOn)
import Html.Keyed
import Json.Decode exposing (succeed)
import Octicons
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image as Image
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Organism.Sidebar exposing (sidebarWidth)
import View.Style exposing (..)


type alias Props c =
    { columns : List c
    , -- Int is index, which is not a strict dependency, but exists for PatterLab
      dragStatus : Int -> c -> DragStatus
    }


type DragStatus
    = Grabbed
    | Droppable
    | Undroppable
    | Settled


type alias Effects c msg =
    { columnDragEnd : msg
    , columnDragStart : Int -> c -> msg
    , columnDragOver : msg
    , columnDragEnter : Int -> msg
    }


render : Effects c msg -> Props c -> Html msg
render eff p =
    Html.Keyed.node "div"
        [ class columnCtnrClass
        , id columnAreaParentId
        , flexRow
        , oneDark
        , on "dragend" (succeed eff.columnDragEnd)
        ]
        (List.indexedMap (columnWrapperKey p.dragStatus eff) p.columns)


columnAreaParentId : String
columnAreaParentId =
    "columnAreaParent"


columnWrapperKey : (Int -> c -> DragStatus) -> Effects c msg -> Int -> c -> ( String, Html msg )
columnWrapperKey dragStatus eff index c =
    let
        staticAttrs =
            [ class columnWrapperClass
            , flexBasisAuto
            , flexColumn
            , Border.w1
            , Border.solid
            , Border.colorBg
            , Background.colorMain
            ]

        dragHandlers =
            case dragStatus index c of
                Grabbed ->
                    [ class grabbedClass
                    , preventDefaultOn "dragover" (succeed ( eff.columnDragOver, True ))
                    ]

                Droppable ->
                    [ class droppableClass
                    , preventDefaultOn "dragenter" (succeed ( eff.columnDragEnter index, True ))
                    , preventDefaultOn "dragover" (succeed ( eff.columnDragOver, True ))
                    ]

                Undroppable ->
                    [ class undroppableClass ]

                Settled ->
                    []
    in
    Tuple.pair ("column_" ++ String.fromInt index) <|
        div (staticAttrs ++ dragHandlers)
            [ header (eff.columnDragStart index c)
            , t (String.fromInt index)
            ]


header : msg -> Html msg
header onDragstart =
    div
        [ class headerClass
        , flexBasisAuto
        , flexRow
        , spacingRow2
        , Background.colorSub
        ]
        [ grabber onDragstart
        , div [ flexGrow ] [ t "header" ]
        ]


grabber : msg -> Html msg
grabber onDragstart =
    div
        [ class grabberClass
        , flexBasisAuto
        , draggable "true"
        , on "dragstart" (succeed onDragstart)
        ]
        [ Octicons.defaultOptions
            |> Octicons.height headerHeight
            |> Octicons.kebabVertical
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
        , ( "transition", "all 0.15s" )
        ]
    , s (c headerClass) [ ( "height", px headerHeight ) ]
    , s (c grabberClass) [ ( "cursor", "all-scroll" ) ]
    , s (c grabbedClass)
        [ ( "transform", "scale(0.98)" )
        , ( "box-shadow", "0px 0px 20px 10px " ++ cssRgba oneDarkTheme.prim )
        ]
    , s (c droppableClass) [ ( "transform", "scale(0.98)" ) ]
    , s (c undroppableClass) [ ( "opacity", "0.2" ) ]
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


headerClass : String
headerClass =
    "chdr"


headerHeight : Int
headerHeight =
    40


grabberClass : String
grabberClass =
    "cgrbbr"


grabbedClass : String
grabbedClass =
    "cgrbbd"


droppableClass : String
droppableClass =
    "cdrppbl"


undroppableClass : String
undroppableClass =
    "cundrppbl"
