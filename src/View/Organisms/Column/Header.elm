module View.Organisms.Column.Header exposing (Effects, render, styles)

import Data.Column as Column
import Html exposing (Attribute, Html, button, div)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (succeed)
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column as Column exposing (ColumnProps)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onColumnDragStart : Bool -> Int -> Column.Id -> msg
    , onHeaderClickWhenScrolled : Column.Id -> msg
    , onPinButtonClick : Column.Id -> Bool -> msg
    , onConfigToggleButtonClick : Column.Id -> Bool -> msg
    , onDismissButtonClick : Int -> msg
    }


type alias Props c =
    ColumnProps
        { c
            | id : Column.Id
            , configOpen : Bool
            , scrolled : Bool
        }


render : Effects msg -> Int -> Props c -> Html msg
render eff index props =
    let
        staticAttrs =
            [ flexRow
            , flexCenter
            , flexBasisAuto
            , padding5
            , spacingRow5
            , Background.colorSub
            ]

        headerClickAttrs =
            if props.scrolled then
                [ onClick (eff.onHeaderClickWhenScrolled props.id), Cursor.pointer ]

            else
                []
    in
    div (staticAttrs ++ headerClickAttrs)
        [ grabbableIcon (eff.onColumnDragStart props.pinned index props.id) props
        , Column.blockTitle [ flexGrow ] props
        , if props.pinned then
            none

          else
            headerButton [ Image.hovSucc ] (eff.onDismissButtonClick index) Octicons.check
        , let
            innerAttrs =
                -- Rotate inner contents, not the button itself, to keep the clickable area stable
                if props.pinned then
                    [ class pinButtonClass, Image.fillWarn, Image.rotate45 ]

                else
                    [ class pinButtonClass ]
          in
          button
            [ flexItem
            , flexBasisAuto
            , noPadding
            , Image.hovWarn
            , Border.round2
            , Background.transparent
            , Background.hovBd
            , onClick (eff.onPinButtonClick props.id (not props.pinned))
            ]
            [ div innerAttrs [ Image.octicon { size = xxProminentSize, shape = Octicons.pin } ] ]
        , headerButton
            [ if props.configOpen then
                Image.fillText

              else
                noAttr
            , Image.hovText
            ]
            (eff.onConfigToggleButtonClick props.id (not props.configOpen))
            Octicons.settings
        ]


headerButton : List (Attribute msg) -> msg -> (Octicons.Options -> Html msg) -> Html msg
headerButton attrs onPress shape =
    let
        baseAttrs =
            [ flexItem
            , flexBasisAuto
            , noPadding
            , Border.round2
            , Background.transparent
            , Background.hovBd
            ]
    in
    Icon.octiconButton (baseAttrs ++ attrs)
        { onPress = onPress
        , size = xxProminentSize
        , shape = shape
        }


grabbableIcon : msg -> Props c -> Html msg
grabbableIcon onColumnDragStart props =
    div
        [ flexBasisAuto
        , draggable "true"
        , on "dragstart" (succeed onColumnDragStart)
        , Cursor.allScroll
        ]
        [ Column.icon30 props
        ]


styles : List Style
styles =
    [ s (c pinButtonClass) [ ( "transition", "transform 0.2s" ) ] ]


pinButtonClass : String
pinButtonClass =
    "pinbtn"
