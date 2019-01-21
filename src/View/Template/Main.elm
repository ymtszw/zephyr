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
import Data.Producer.Discord as Discord
import Html exposing (Attribute, Html, div, h2, img)
import Html.Attributes exposing (alt, class, draggable, id, src)
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
import View.Molecule.Wallpaper as Wallpaper
import View.Organism.Sidebar as Sidebar exposing (sidebarWidth)
import View.Style exposing (..)


type alias Props c =
    { sidebarProps : Sidebar.Props
    , configDrawerIsOpen : Bool
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
    { configContents : ConfigContents msg
    , columnContents : ColumnContents c msg
    }


type alias ConfigContents msg =
    { pref : Html msg
    , discord : Html msg
    }


type alias ColumnContents c msg =
    { header : Int -> c -> Html msg
    , config : Int -> c -> Html msg
    , newMessageEditor : c -> Html msg
    , items : c -> Html msg
    }


render : Effects c msg -> Props c -> Contents c msg -> List (Html msg)
render eff p contents =
    -- XXX Order matters! Basically, elements are stacked in written order unless specified otherwise (via z-index)
    [ Wallpaper.zephyr
    , columnContainer eff.columnCtnrEffects p.columnCtnrProps contents.columnContents
    , configDrawer p.configDrawerIsOpen contents.configContents
    , Sidebar.render eff.sidebarEffects p.sidebarProps
    ]


configDrawer : Bool -> ConfigContents msg -> Html msg
configDrawer isOpen cc =
    div
        [ class configDrawerClass
        , if isOpen then
            class drawerOpenClass

          else
            noAttr
        , oneDark
        , flexColumn
        , padding15
        , spacingColumn10
        , Background.colorBg
        ]
        [ configSectionWrapper Nothing prefTitle cc.pref
        , configSectionWrapper Nothing discordTitle cc.discord
        ]


configSectionWrapper : Maybe (Attribute msg) -> Html msg -> Html msg -> Html msg
configSectionWrapper maybeTheme title content =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        , Border.round5
        , Background.colorMain
        , case maybeTheme of
            Just theme ->
                theme

            Nothing ->
                noAttr
        ]
        [ title
        , content
        ]


prefTitle : Html msg
prefTitle =
    titleTemplate "Preference" <|
        Image.octicon
            { size = titleIconSize
            , shape = Octicons.settings
            }


titleTemplate : String -> Html msg -> Html msg
titleTemplate text icon =
    h2
        [ class configTitleClass
        , padding5
        , bold
        , sizeTitle
        , Border.solid
        ]
        [ icon
        , t " "
        , t text
        ]


titleIconSize : Int
titleIconSize =
    -- same as sizeTitle
    18


discordTitle : Html msg
discordTitle =
    titleTemplate "Discord" <|
        img
            [ class discordLogoClass
            , Border.round5
            , src (Discord.defaultIconUrl (Just titleIconSize))
            , alt "Discord logo"
            ]
            []


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
    [ s (c configDrawerClass)
        [ ( "position", "fixed" )
        , ( "left", px sidebarWidth )
        , ( "top", "0" )
        , ( "width", px configDrawerWidth )
        , ( "height", "100vh" )
        , ( "max-height", "100vh" )
        , ( "overflow-y", "auto" )
        , ( "transition", "all 0.15s" )
        , -- Default hidden
          ( "visibility", "hidden" )
        , ( "opacity", "0" )
        , ( "transform", "translateX(-50px)" ) -- The value sufficient for slide-in effect to be recognizable
        ]
    , s (c configDrawerClass ++ c drawerOpenClass)
        [ ( "display", "block" )
        , ( "visibility", "visible" )
        , ( "opacity", "1" )
        , ( "transform", "translateX(0px)" )
        ]
    , s (c configDrawerClass ++ " " ++ c configTitleClass) [ ( "border-bottom-width", "1px" ) ]
    , s (c configDrawerClass ++ " " ++ c discordLogoClass) [ ( "width", px titleIconSize ), ( "height", px titleIconSize ) ]
    , s (c columnCtnrClass)
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


configDrawerClass : String
configDrawerClass =
    "cnfdrwr"


configDrawerWidth : Int
configDrawerWidth =
    640


drawerOpenClass : String
drawerOpenClass =
    "drwropen"


configTitleClass : String
configTitleClass =
    "cnftitle"


discordLogoClass : String
discordLogoClass =
    "cnfdiscord"


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
