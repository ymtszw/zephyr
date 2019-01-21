module View.Template.Main exposing
    ( Props, Effects, Contents, DragStatus(..), render
    , styles
    )

{-| Template of Main view.

It consumes necessary data as Props, which is effectively a ViewModel.

Also, you can inject event handlers as a set of functions (called Effects).
This allows you to wire different functions for any parts,
so that we can test our views in PatternLab! DI, anyone?

And finally, Contents record aggregates actual contents to be placed in the template.

@docs Props, Effects, Contents, DragStatus, render
@docs styles

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
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Wallpaper as Wallpaper
import View.Organism.Sidebar as Sidebar exposing (sidebarWidth)
import View.Style exposing (..)


type alias Props c =
    { sidebarProps : Sidebar.Props
    , columnCtnrProps : ColumnContainerProps c
    }


type alias ColumnContainerProps c =
    { visibleColumns : List c
    , -- Int is index, which is not a strict dependency, but exists for PatterLab
      dragStatus : Int -> c -> DragStatus
    }


type DragStatus
    = Grabbed
    | Droppable
    | Undroppable
    | Settled


type alias Effects c msg =
    { sidebarEffects : Sidebar.Effects msg
    , columnCtnrEffects : ColumnContainerEffects c msg
    }


type alias ColumnContainerEffects c msg =
    { columnDragEnd : msg
    , columnDragStart : Int -> c -> msg
    , columnDragOver : msg
    , columnDragEnter : Int -> msg
    }


type alias Contents c msg =
    { columnContents : ColumnContents c msg
    }


type alias ColumnContents c msg =
    { header : Int -> c -> Html msg
    , config : Int -> c -> Html msg
    , newMessageEditor : c -> Html msg
    , items : c -> Html msg
    }


render : Effects c msg -> Props c -> Contents c msg -> List (Html msg)
render eff p contents =
    [ Wallpaper.zephyr
    , Sidebar.render eff.sidebarEffects p.sidebarProps
    , columnContainer eff.columnCtnrEffects p.columnCtnrProps contents.columnContents
    ]


columnContainer :
    ColumnContainerEffects c msg
    -> ColumnContainerProps c
    -> ColumnContents c msg
    -> Html msg
columnContainer eff p contents =
    Html.Keyed.node "div"
        [ class columnCtnrClass
        , id columnAreaParentId
        , flexRow
        , oneDark
        , on "dragend" (succeed eff.columnDragEnd)
        ]
        (List.indexedMap (columnWrapperKey p.dragStatus eff contents) p.visibleColumns)


columnAreaParentId : String
columnAreaParentId =
    "columnAreaParent"


columnWrapperKey :
    (Int -> c -> DragStatus)
    -> ColumnContainerEffects c msg
    -> ColumnContents c msg
    -> Int
    -> c
    -> ( String, Html msg )
columnWrapperKey dragStatus eff contents index c =
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
            [ header (eff.columnDragStart index c) (contents.header index c)
            , contents.config index c
            , contents.newMessageEditor c
            , div
                [ class itemsWrapperClass
                , flexBasisAuto
                , flexShrink
                ]
                [ contents.items c ]
            ]


header : msg -> Html msg -> Html msg
header onDragstart content =
    div
        [ class headerClass
        , flexBasisAuto
        , flexRow
        , spacingRow2
        , Background.colorSub
        ]
        [ grabber onDragstart
        , div [ flexGrow ] [ content ]
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
        , ( "max-height", "100vh" )
        , ( "overflow-y", "auto" )
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
    , s (c itemsWrapperClass)
        [ ( "max-height", "" )
        , ( "overflow-y", "auto" )
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


itemsWrapperClass : String
itemsWrapperClass =
    "citmswrap"
