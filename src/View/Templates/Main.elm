module View.Templates.Main exposing
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
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html, div, img, span)
import Html.Attributes exposing (alt, class, id, src)
import Html.Events exposing (on, preventDefaultOn)
import Html.Keyed
import Json.Decode exposing (succeed)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Theme exposing (aubergine, oneDark, oneDarkTheme)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.Source exposing (Source(..))
import View.Molecules.Wallpaper as Wallpaper
import View.Organisms.Sidebar as Sidebar exposing (ColumnInSidebar, sidebarExpansionWidth, sidebarWidth)
import View.Style exposing (..)


type alias Props c =
    { configOpen : Bool
    , visibleColumns : List (VisibleColumn c)
    }


type alias VisibleColumn c =
    ColumnInSidebar
        { c
            | dragStatus : DragStatus
            , configOpen : Bool
        }


type DragStatus
    = Grabbed
    | Droppable
    | Undroppable
    | Settled


type alias Effects msg =
    { sidebarEffects : Sidebar.Effects msg
    , -- DragStart is handled in Organisms.Column.Header
      columnDragEnd : msg
    , columnDragOver : msg
    , columnDragEnter : Int -> msg
    }


type alias Contents c msg =
    { configContents : ConfigContents msg
    , columnContents : ColumnContents c msg
    }


type alias ConfigContents msg =
    { pref : Html msg
    , slack : Html msg
    , discord : Html msg
    , status : Html msg
    }


type alias ColumnContents c msg =
    { header : Int -> VisibleColumn c -> Html msg
    , config : Int -> VisibleColumn c -> Html msg
    , newMessageEditor : VisibleColumn c -> Html msg
    , items : VisibleColumn c -> Html msg
    }


render : Effects msg -> Props c -> Contents c msg -> List (Html msg)
render eff props contents =
    -- XXX Order matters! Basically, elements are stacked in written order unless specified otherwise (via z-index)
    [ Wallpaper.zephyr
    , columnContainer eff props.visibleColumns contents.columnContents
    , configDrawer props.configOpen contents.configContents
    , Sidebar.render eff.sidebarEffects props
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
        , spacingColumn15
        , Background.colorBg
        ]
        [ configSectionWrapper Nothing prefTitle cc.pref
        , configSectionWrapper (Just aubergine) slackTitle cc.slack
        , configSectionWrapper Nothing discordTitle cc.discord
        , configSectionWrapper Nothing statusTitle cc.status
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
        span [ Image.fillText ] [ Image.octicon { size = xProminentSize, shape = Octicons.settings } ]


titleTemplate : String -> Html msg -> Html msg
titleTemplate text icon =
    div
        [ flexRow
        , flexCenter
        , spacingRow5
        , padding5
        , Border.solid
        , Border.bot1
        ]
        [ icon
        , div [ bold, xProminent ] [ t text ]
        ]


slackTitle : Html msg
slackTitle =
    titleTemplate "Slack" (Icon.slack20 [])


discordTitle : Html msg
discordTitle =
    titleTemplate "Discord" (Icon.discord20 [])


statusTitle : Html msg
statusTitle =
    titleTemplate "Status" <|
        span [ Image.fillSucc ] [ Image.octicon { size = xProminentSize, shape = Octicons.pulse } ]


columnContainer :
    Effects msg
    -> List (VisibleColumn c)
    -> ColumnContents c msg
    -> Html msg
columnContainer eff visibleColumns contents =
    Html.Keyed.node "div"
        [ class columnCtnrClass
        , id columnAreaParentId
        , flexRow
        , oneDark
        , on "dragend" (succeed eff.columnDragEnd)
        ]
        (List.indexedMap (columnWrapperKey eff contents) visibleColumns)


columnAreaParentId : String
columnAreaParentId =
    "columnAreaParent"


columnWrapperKey :
    Effects msg
    -> ColumnContents c msg
    -> Int
    -> VisibleColumn c
    -> ( String, Html msg )
columnWrapperKey eff contents index c =
    let
        staticAttrs =
            [ class columnWrapperClass
            , flexBasisAuto
            , flexColumn
            , Border.w1
            , Border.solid
            , Border.colorBg
            , Background.colorMain
            , case c.sources of
                (SlackSource _) :: _ ->
                    aubergine

                _ ->
                    noAttr
            ]

        dragHandlers =
            case c.dragStatus of
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
            [ contents.header index c
            , if c.configOpen then
                contents.config index c

              else
                none
            , contents.newMessageEditor c
            , div
                [ class itemsWrapperClass
                , flexBasisAuto
                , flexShrink
                ]
                [ contents.items c ]
            ]


styles : List Style
styles =
    [ s (c configDrawerClass)
        [ ( "position", "fixed" )
        , ( "left", px (sidebarWidth + sidebarExpansionWidth) )
        , ( "top", "0" )
        , ( "width", px configDrawerWidth )
        , ( "height", "100vh" )
        , ( "max-height", "100vh" )
        , ( "padding-top", px configDrawerPaddingY )
        , ( "padding-left", px configDrawerPaddingLeft )
        , ( "padding-right", px configDrawerPaddingRight )
        , ( "padding-bottom", px configDrawerPaddingY )
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
    , s (c grabbedClass)
        [ ( "transform", "scale(0.98)" )
        , ( "box-shadow", "0px 0px 20px 10px " ++ cssRgba oneDarkTheme.prim )
        ]
    , s (c droppableClass) [ ( "transform", "scale(0.98)" ) ]
    , s (c undroppableClass) [ ( "opacity", "0.2" ) ]
    , s (c itemsWrapperClass) [ ( "overflow-y", "auto" ) ]
    ]


configDrawerClass : String
configDrawerClass =
    "cnfdrwr"


configDrawerWidth : Int
configDrawerWidth =
    640


configDrawerPaddingY : Int
configDrawerPaddingY =
    20


configDrawerPaddingLeft : Int
configDrawerPaddingLeft =
    5


configDrawerPaddingRight : Int
configDrawerPaddingRight =
    10


drawerOpenClass : String
drawerOpenClass =
    "drwropen"


columnCtnrClass : String
columnCtnrClass =
    "cctnr"


columnWrapperClass : String
columnWrapperClass =
    "cwrap"


columnWidth : Int
columnWidth =
    350


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
