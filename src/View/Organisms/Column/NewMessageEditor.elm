module View.Organisms.Column.NewMessageEditor exposing (Effects, Props, render, selectId, styles)

import Data.Column as Column
import Data.ColumnEditor exposing (ColumnEditor(..), UserAction(..), getBuffer)
import File exposing (File)
import Html exposing (Attribute, Html, button, div, img, span, textarea)
import Html.Attributes exposing (alt, class, disabled, id, placeholder, spellcheck, src, title)
import Html.Events exposing (on, onClick, onFocus, onInput, preventDefaultOn)
import Html.Keyed
import Id
import Json.Decode as D
import Json.DecodeExtra as D
import List.Extra
import Octicons
import SelectArray exposing (SelectArray)
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip, ellipsis, nowrap)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column exposing (ColumnProps)
import View.Molecules.Icon as Icon
import View.Molecules.Source as Source exposing (Source(..))
import View.Style exposing (..)


type alias Effects msg =
    { onEditorSelect : Column.Id -> Int -> msg
    , selectMsgTagger : Select.Msg msg -> msg
    , onTextInput : Column.Id -> String -> msg
    , -- Used for dragenter and dragover too
      onInteracted : Column.Id -> UserAction -> msg
    , onResetButtonClick : Column.Id -> msg
    , -- TODO support multiple files in a editor
      onDiscardFileButtonClick : Column.Id -> msg
    , -- XXX should diverge for different sources later
      onRequestFileAreaClick : Column.Id -> msg
    , onFileDrop : Column.Id -> File -> msg
    , onSubmit : Column.Id -> msg
    }


type alias Props c =
    { selectState : Select.State
    , column :
        ColumnProps
            { c
                | id : Column.Id
                , editors : SelectArray ColumnEditor
                , editorSeq : Int -- Force triggering DOM generation when incremented; workaround for https://github.com/elm/html/issues/55
                , userActionOnEditor : UserAction
            }
    }


render : Effects msg -> Props c -> Html msg
render eff props =
    let
        selectedEditor =
            SelectArray.selected props.column.editors

        isActive =
            case props.column.userActionOnEditor of
                OutOfFocus ->
                    False

                _ ->
                    True
    in
    -- Workaround for https://github.com/elm/html/issues/55
    Html.Keyed.node "div"
        [ flexColumn
        , flexBasisAuto
        , padding5
        , spacingColumn5
        , Border.colorNote
        , Border.bot1
        , Border.solid
        ]
        [ ( "editorMenu_" ++ Id.to props.column.id, editorMenu eff props isActive selectedEditor )
        , ( "editorTextarea_" ++ Id.to props.column.id ++ "_" ++ String.fromInt props.column.editorSeq
          , editorTextarea eff props.column.id isActive selectedEditor
          )
        , ( "selectedFiles_" ++ Id.to props.column.id, selectedFiles eff props.column isActive selectedEditor )
        , ( "submitButton_" ++ Id.to props.column.id, submitButton (eff.onSubmit props.column.id) isActive selectedEditor )
        ]


editorMenu : Effects msg -> Props c -> Bool -> ColumnEditor -> Html msg
editorMenu eff props isActive editor =
    if isActive then
        div [ flexRow, spacingRow5, flexCenter ]
            [ div [] [ Image.octicon { size = prominentSize, shape = Octicons.pencil } ]
            , editorSelect eff props editor
            , Icon.octiconButton [ flexItem, padding2, Background.transparent, Background.hovBd, pushRight, Image.hovErr ]
                { onPress = eff.onResetButtonClick props.column.id, size = prominentSize, shape = Octicons.trashcan }
            , Icon.octiconButton [ flexItem, padding2, Background.transparent, Background.hovBd, Image.hovText ]
                { onPress = eff.onInteracted props.column.id OutOfFocus, size = prominentSize, shape = Octicons.x }
            ]

    else
        none


editorSelect : Effects msg -> Props c -> ColumnEditor -> Html msg
editorSelect eff props editor =
    let
        indexedEditors =
            SelectArray.indexedMap (\{ index, e } -> ( String.fromInt index, ( index, e ) )) props.column.editors

        selectedIndex =
            SelectArray.selectedIndex props.column.editors
    in
    Select.render [ class editorSelectClass, flexItem, flexShrink, flexBasisAuto ]
        { state = props.selectState
        , msgTagger = eff.selectMsgTagger
        , thin = True
        , id = selectId props.column.id
        , onSelect = \( newIndex, _ ) -> eff.onEditorSelect props.column.id newIndex
        , selectedOption = Just ( selectedIndex, editor )
        , filterMatch = Nothing
        , options = indexedEditors
        , optionHtml = editorSelectOption props.column.sources
        }


selectId : Column.Id -> String
selectId columnId =
    "editorSelect_" ++ Id.to columnId


editorSelectOption : List Source -> ( Int, ColumnEditor ) -> Html msg
editorSelectOption sources ( _, editor ) =
    case editor of
        DiscordMessageEditor { channelId } ->
            let
                matchingDiscordSource s =
                    case s of
                        DiscordSource { id } ->
                            id == Id.to channelId

                        _ ->
                            False
            in
            case List.Extra.find matchingDiscordSource sources of
                Just s ->
                    Source.horizontalBlock14 s

                Nothing ->
                    t (Id.to channelId)

        LocalMessageEditor _ ->
            t "Personal Memo"


editorTextarea : Effects msg -> Column.Id -> Bool -> ColumnEditor -> Html msg
editorTextarea eff cId isActive editor =
    let
        buffer =
            getBuffer editor

        baseAttrs =
            [ class textareaClass
            , id (Column.editorId cId) -- For Dom.Blur
            , flexItem
            , widthFill
            , padding5
            , spellcheck True
            , placeholder placeholder_
            , Border.round5
            , onFocus (eff.onInteracted cId Authoring)
            , onInput (eff.onTextInput cId)
            , textareaKeyBinds (isNotReadyToSubmit editor) (eff.onInteracted cId OutOfFocus) (eff.onSubmit cId)
            ]

        placeholder_ =
            let
                ctrlEnterInstruction base =
                    if isActive then
                        base ++ "  (Ctrl + Enter to submit)"

                    else
                        base
            in
            ctrlEnterInstruction <|
                case editor of
                    DiscordMessageEditor _ ->
                        "Message"

                    LocalMessageEditor _ ->
                        "Memo"

        stateAttrs =
            if isActive then
                let
                    bufferHeight =
                        if lines < 6 then
                            regularSize * 8

                        else
                            regularSize * 16

                    lines =
                        List.length (String.split "\n" buffer)
                in
                [ flexBasis (px bufferHeight), colorText, phColorText, Background.colorNote ]

            else
                [ flexBasis (px (regularSize * 2)), colorNote, phColorNote, Background.colorSub ]
    in
    textarea (baseAttrs ++ stateAttrs) [ t buffer ]


isNotReadyToSubmit : ColumnEditor -> Bool
isNotReadyToSubmit editor =
    case editor of
        DiscordMessageEditor opts ->
            String.isEmpty opts.buffer && opts.file == Nothing

        LocalMessageEditor buffer ->
            String.isEmpty buffer


textareaKeyBinds : Bool -> msg -> msg -> Attribute msg
textareaKeyBinds notReady blur submit =
    -- XXX Not using D.oneOf; https://github.com/elm/html/issues/180
    on "keydown" <|
        D.do (D.field "key" D.string) <|
            \key ->
                if key == "Escape" then
                    D.succeed blur

                else if notReady then
                    D.fail "stop here"

                else if key == "Enter" then
                    D.when (D.field "ctrlKey" D.bool) identity (D.succeed submit)

                else
                    D.fail "not bound"


selectedFiles : Effects msg -> { c | id : Column.Id, userActionOnEditor : UserAction } -> Bool -> ColumnEditor -> Html msg
selectedFiles eff c isActive editor =
    case ( isActive, editor ) of
        ( True, DiscordMessageEditor { file } ) ->
            case file of
                Just ( f, dataUrl ) ->
                    filePreview (eff.onDiscardFileButtonClick c.id) f dataUrl

                Nothing ->
                    fileSelectArea eff c

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
                    , Image.hovText
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
        , flexBasisAuto
        , Border.round5
        , Background.colorSub
        ]
        { topRight = Just discardFileButton
        , bottomRight = Just fileMetadata
        , content = div [ widthFill, flexColumn, flexBasisAuto, flexCenter ] [ actualPreview ]
        }


fileSelectArea : Effects msg -> { c | id : Column.Id, userActionOnEditor : UserAction } -> Html msg
fileSelectArea eff c =
    let
        staticAttrs =
            [ flexItem
            , padding5
            , Border.w1
            , Border.round5
            , Border.dashed
            , bg
            , Background.hovBd
            , fill
            , Image.hovText
            ]

        fileDropper =
            case c.userActionOnEditor of
                OutOfFocus ->
                    []

                _ ->
                    [ checkHoveredObjectHasFiles "dragenter"
                    , checkHoveredObjectHasFiles "dragover"
                    , hijackOn "dragleave" (D.succeed (eff.onInteracted c.id Authoring))
                    , hijackOn "drop" catchDroppedFile
                    ]

        checkHoveredObjectHasFiles event =
            -- XXX event.dataTransfer.files may be empty on hover events, but resolved on drop event
            -- Also, D.oneOrMore/list APIs do NOT support all Array-like JS data structure.
            -- In this case, DataTransferItemList object cannot be decoded in Array-like manner.
            let
                itemsDecoder =
                    D.when (D.at [ "0", "kind" ] D.string) ((==) "file") <|
                        D.succeed ( eff.onInteracted c.id HoveringFiles, True )
            in
            preventDefaultOn event <|
                D.oneOf
                    [ D.at [ "dataTransfer", "items" ] itemsDecoder
                    , D.succeed ( eff.onInteracted c.id HoveringNonFile, False )
                    ]

        catchDroppedFile =
            D.oneOf
                [ D.at [ "dataTransfer", "files" ] (D.oneOrMore (\f _ -> eff.onFileDrop c.id f) File.decoder)
                , D.succeed (eff.onInteracted c.id Authoring)
                ]

        hijackOn event msgDecoder =
            preventDefaultOn event (D.map (\msg -> ( msg, True )) msgDecoder)

        ( bg, fill, shape ) =
            case c.userActionOnEditor of
                HoveringFiles ->
                    ( Background.colorSucc, Image.fillText, Octicons.check )

                HoveringNonFile ->
                    ( Background.colorErr, Image.fillText, Octicons.circleSlash )

                _ ->
                    ( Background.transparent, noAttr, Octicons.cloudUpload )
    in
    Icon.octiconButton (staticAttrs ++ fileDropper)
        { onPress = eff.onRequestFileAreaClick c.id
        , size = xProminentSize
        , shape = shape
        }


submitButton : msg -> Bool -> ColumnEditor -> Html msg
submitButton onSubmit isActive editor =
    if isActive then
        button
            [ flexItem
            , alignEnd
            , padding5
            , disabled (isNotReadyToSubmit editor)
            , Background.colorSucc
            , onClick onSubmit
            ]
            [ t "Submit" ]

    else
        none



-- STYLES


styles : List Style
styles =
    [ s (c editorSelectClass) [ ( "width", px fixedEditorSelectWidth ) ]
    , s (c textareaClass)
        [ ( "resize", "none" )
        , ( "transition", "all 0.15s" )
        ]
    , s (c previewImageClass)
        [ ( "max-width", "100%" )
        , ( "max-height", px maxPreviewHeight )
        , ( "min-height", px minPreviewHeight )
        , ( "object-fit", "scale-down" )
        ]
    , s (c previewMetadataClass) [ ( "max-width", px maxMetadataWidth ) ]
    ]


editorSelectClass : String
editorSelectClass =
    "editorSelect"


fixedEditorSelectWidth : Int
fixedEditorSelectWidth =
    200


textareaClass : String
textareaClass =
    "editortextarea"


previewImageClass : String
previewImageClass =
    "editorpreviewimg"


maxPreviewHeight : Int
maxPreviewHeight =
    400


minPreviewHeight : Int
minPreviewHeight =
    -- Required height for force-showing discard button and metadata
    100


previewMetadataClass : String
previewMetadataClass =
    "editorpreviewmd"


maxMetadataWidth : Int
maxMetadataWidth =
    250
