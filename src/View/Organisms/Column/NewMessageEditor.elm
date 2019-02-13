module View.Organisms.Column.NewMessageEditor exposing (render, styles)

import Data.ColumnEditor exposing (ColumnEditor(..), getBuffer)
import File exposing (File)
import Html exposing (Html, button, div, img, textarea)
import Html.Attributes exposing (alt, class, placeholder, spellcheck, src)
import Html.Events exposing (onFocus, onInput)
import Html.Keyed
import Octicons
import SelectArray exposing (SelectArray)
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onTextInput : String -> String -> msg
    , onToggleActive : String -> Bool -> msg
    , onResetButtonClick : String -> msg
    , -- TODO support multiple files in a editor
      onDeleteFileButtonClick : String -> msg
    }


type alias ColumnProps c =
    { c
        | id : String
        , editors : SelectArray ColumnEditor
        , editorSeq : Int -- Force triggering DOM generation when incremented; workaround for https://github.com/elm/html/issues/55
        , editorActive : Bool
    }


render : Effects msg -> ColumnProps c -> Html msg
render eff c =
    let
        selectedEditor =
            SelectArray.selected c.editors
    in
    -- Workaround for https://github.com/elm/html/issues/55
    Html.Keyed.node "div"
        [ flexColumn
        , padding5
        , spacingColumn5
        , Border.colorBd
        , Border.bot1
        , Border.solid
        ]
        [ ( "editorMenu_" ++ c.id, editorMenu eff c selectedEditor )
        , ( "editorTextarea_" ++ c.id ++ "_" ++ String.fromInt c.editorSeq
          , editorTextarea eff c selectedEditor
          )
        , ( "selectedFiles_" ++ c.id, selectedFiles eff c selectedEditor )
        ]


editorMenu : Effects msg -> ColumnProps c -> ColumnEditor -> Html msg
editorMenu eff c editor =
    if c.editorActive then
        div [ flexRow, spacingRow5, flexCenter ]
            [ div [] [ Image.octicon { size = prominentSize, shape = Octicons.pencil } ]
            , Icon.octiconButton [ flexItem, padding2, Background.transparent, Background.hovBd, pushRight, Image.hovErr ]
                { onPress = eff.onResetButtonClick c.id, size = prominentSize, shape = Octicons.trashcan }
            , Icon.octiconButton [ flexItem, padding2, Background.transparent, Background.hovBd, Image.hovText ]
                { onPress = eff.onToggleActive c.id False, size = prominentSize, shape = Octicons.x }
            ]

    else
        none


editorTextarea : Effects msg -> ColumnProps c -> ColumnEditor -> Html msg
editorTextarea eff c editor =
    let
        buffer =
            getBuffer editor

        baseAttrs =
            [ class textareaClass
            , flexItem
            , widthFill
            , padding5
            , spellcheck True
            , placeholder <|
                case editor of
                    DiscordMessageEditor _ ->
                        "Message"

                    LocalMessageEditor _ ->
                        "Memo"
            , Border.round5
            , onFocus (eff.onToggleActive c.id True)
            , onInput (eff.onTextInput c.id)
            ]

        stateAttrs =
            if c.editorActive then
                let
                    bufferHeight =
                        if lines < 6 then
                            regularSize * 8

                        else
                            regularSize * 16

                    lines =
                        List.length (String.split "\n" buffer)
                in
                [ flexBasis (px bufferHeight), colorText, Background.colorNote ]

            else
                [ flexBasis (px (regularSize * 2)), colorNote, Background.colorSub ]
    in
    textarea (baseAttrs ++ stateAttrs) [ t buffer ]


selectedFiles : Effects msg -> ColumnProps c -> ColumnEditor -> Html msg
selectedFiles eff c editor =
    case ( c.editorActive, editor ) of
        ( True, DiscordMessageEditor { file } ) ->
            case file of
                Just ( f, dataUrl ) ->
                    withPreviewWrapper (eff.onDeleteFileButtonClick c.id) f <|
                        if String.startsWith "image/" (File.mime f) then
                            img [ block, src dataUrl, alt (File.name f) ] []

                        else
                            div [ padding15 ] [ Image.octicon { size = xxProminentSize, shape = Octicons.file } ]

                Nothing ->
                    -- TODO droppable area
                    none

        _ ->
            none


withPreviewWrapper : msg -> File -> Html msg -> Html msg
withPreviewWrapper onDeleteFileButtonClick f preview =
    let
        deleteFileButton =
            div [ Background.transparent, padding5 ]
                [ Icon.octiconButton
                    [ padding5
                    , Border.elliptic
                    , Background.colorSub
                    ]
                    { onPress = onDeleteFileButtonClick
                    , size = prominentSize
                    , shape = Octicons.x
                    }
                ]
    in
    withBadge
        [ Border.round5
        , Background.colorSub
        ]
        { topRight = Just deleteFileButton
        , bottomRight = Nothing
        , content = preview
        }



-- STYLES


styles : List Style
styles =
    [ s (c textareaClass)
        [ ( "resize", "none" )
        , ( "transition", "all 0.15s" )
        ]
    ]


textareaClass : String
textareaClass =
    "editortextarea"
