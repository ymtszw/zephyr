module View.Organisms.Column.NewMessageEditor exposing (render, styles)

import Data.ColumnEditor exposing (ColumnEditor(..), getBuffer)
import File exposing (File)
import Html exposing (Html, button, div, img, span, textarea)
import Html.Attributes exposing (alt, class, placeholder, spellcheck, src, title)
import Html.Events exposing (onFocus, onInput)
import Html.Keyed
import Octicons
import SelectArray exposing (SelectArray)
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip, ellipsis, nowrap)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onTextInput : String -> String -> msg
    , onToggleActive : String -> Bool -> msg
    , onResetButtonClick : String -> msg
    , -- TODO support multiple files in a editor
      onDiscardFileButtonClick : String -> msg
    , -- XXX should diverge for different sources later
      onRequestFileAreaClick : String -> msg
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
                    filePreview (eff.onDiscardFileButtonClick c.id) f dataUrl

                Nothing ->
                    fileSelectArea (eff.onRequestFileAreaClick c.id)

        _ ->
            none


filePreview : msg -> File -> String -> Html msg
filePreview onDiscardFileButtonClick f dataUrl =
    let
        actualPreview =
            case String.split "/" (File.mime f) of
                "image" :: _ ->
                    img [ class previewImageClass, block, src dataUrl, alt (File.name f) ] []

                "video" :: _ ->
                    fileIconPreview Octicons.deviceCameraVideo

                "application" :: appSubType :: _ ->
                    if appSubType == "pdf" then
                        fileIconPreview Octicons.filePdf

                    else if appSubType == "octet-stream" then
                        fileIconPreview Octicons.fileBinary

                    else if isArchive appSubType then
                        fileIconPreview Octicons.fileZip

                    else
                        -- Can do more, but stopping right here
                        fileIconPreview Octicons.file

                _ ->
                    fileIconPreview Octicons.file

        fileIconPreview shape =
            div [ padding15 ] [ Image.octicon { size = xxxProminentSize, shape = shape } ]

        isArchive appSubType =
            List.member appSubType
                [ "zip", "x-bzip", "x-bzip2", "x-gzip", "x-rar-compressed", "x-7z-compressed", "x-tar" ]

        discardFileButton =
            div [ Background.transparent, padding5 ]
                [ Icon.octiconButton
                    [ padding5
                    , Border.elliptic
                    , Background.colorBg
                    , Background.hovBd
                    ]
                    { onPress = onDiscardFileButtonClick
                    , size = prominentSize
                    , shape = Octicons.x
                    }
                ]

        fileMetadata =
            let
                metadata title_ =
                    div
                        [ class previewMetadataClass
                        , padding5
                        , alignEnd
                        , nowrap
                        , clip
                        , ellipsis
                        , title title_
                        , Border.round5
                        , Background.colorBg
                        ]
            in
            div [ flexColumn, spacingColumn2, Background.transparent, padding5 ]
                [ metadata (File.name f) [ span [ bold ] [ t (File.name f) ] ]
                , let
                    mimeAndSize =
                        mimeOrQuestion ++ " (" ++ StringExtra.punctuateNumber (File.size f) ++ " Bytes)"

                    mimeOrQuestion =
                        case File.mime f of
                            "" ->
                                "?"

                            nonEmptyMime ->
                                nonEmptyMime
                  in
                  metadata mimeAndSize [ t mimeAndSize ]
                ]
    in
    withBadge
        [ clip
        , Border.round5
        , Background.colorSub
        ]
        { topRight = Just discardFileButton
        , bottomRight = Just fileMetadata
        , content = div [ widthFill, flexColumn, flexCenter ] [ actualPreview ]
        }


fileSelectArea : msg -> Html msg
fileSelectArea onRequestFileAreaClick =
    Icon.octiconButton
        [ flexItem
        , padding5
        , Border.w1
        , Border.round5
        , Border.dashed
        , Background.transparent
        , Background.hovBd
        , Image.hovText
        ]
        { onPress = onRequestFileAreaClick
        , size = xProminentSize
        , shape = Octicons.plus
        }



-- STYLES


styles : List Style
styles =
    [ s (c textareaClass)
        [ ( "resize", "none" )
        , ( "transition", "all 0.15s" )
        ]
    , s (c previewImageClass)
        [ ( "max-width", "100%" )
        , ( "max-height", px maxPreviewHeight )
        , ( "object-fit", "scale-down" )
        ]
    , s (c previewMetadataClass) [ ( "max-width", px maxMetadataWidth ) ]
    ]


textareaClass : String
textareaClass =
    "editortextarea"


previewImageClass : String
previewImageClass =
    "editorpreviewimg"


maxPreviewHeight : Int
maxPreviewHeight =
    400


previewMetadataClass : String
previewMetadataClass =
    "editorpreviewmd"


maxMetadataWidth : Int
maxMetadataWidth =
    250
