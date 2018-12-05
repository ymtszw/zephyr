module View.NewMessageEditor exposing (newMessageEditorEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnEditor exposing (ColumnEditor(..), CommonEditorOpts)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events exposing (onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input
import Element.Keyed
import File exposing (File)
import Html.Attributes exposing (placeholder)
import ListExtra
import Octicons
import SelectArray exposing (SelectArray)
import StringExtra
import View.Parts exposing (..)
import View.Select as Select


newMessageEditorEl : Int -> Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
newMessageEditorEl clientHeight ss fam c =
    let
        selectedEditor =
            SelectArray.selected c.editors

        isActive =
            editorIsActive selectedEditor
    in
    column
        [ width fill
        , height shrink
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BD.color oneDark.bd
        , BD.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        ]
        [ row [ width fill, spacing spacingUnit, centerY, visible isActive ]
            [ octiconEl [] { size = editorHeaderIconSize, color = defaultOcticonColor, shape = Octicons.pencil }
            , editorSelectEl ss fam c
            , editorResetButtonEl c.id
            ]
        , textAreaInputEl isActive c.id selectedEditor
        , selectedFilesEl c.id selectedEditor
        , editorButtonsEl clientHeight isActive c.id selectedEditor
        ]


editorHeaderIconSize : Int
editorHeaderIconSize =
    scale12 2


editorIsActive : ColumnEditor -> Bool
editorIsActive ce =
    case ce of
        DiscordMessageEditor { file } opts ->
            opts.active

        LocalMessageEditor opts ->
            opts.active


editorSelectEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
editorSelectEl ss fam c =
    let
        indexedEditors =
            SelectArray.indexedMap (\{ index, e } -> ( String.fromInt index, ( index, e ) )) c.editors

        selectedIndex =
            SelectArray.selectedIndex c.editors
    in
    Select.select
        [ width (px editorSelectWidth)
        , height shrink
        , paddingEach { top = 0, right = 0, bottom = 0, left = rectElementInnerPadding }
        , Font.size editorFontSize
        ]
        { state = ss
        , id = editorSelectId c.id
        , theme = oneDark
        , onSelect = onEditorSelect c.id selectedIndex
        , selectedOption = Just ( selectedIndex, SelectArray.selected c.editors )
        , options = indexedEditors
        , optionEl = editorSelectOptionEl fam
        }


editorSelectWidth : Int
editorSelectWidth =
    150


editorSelectId : String -> String
editorSelectId cId =
    cId ++ "_newMessageEditorSelect"


onEditorSelect : String -> Int -> ( Int, ColumnEditor ) -> Msg
onEditorSelect cId selectedIndex ( index, _ ) =
    if index == selectedIndex then
        NoOp

    else
        ColumnCtrl cId (Column.SelectEditor index)


editorSelectOptionEl : FilterAtomMaterial -> ( Int, ColumnEditor ) -> Element Msg
editorSelectOptionEl fam ( _, ce ) =
    case ce of
        DiscordMessageEditor { channelId } _ ->
            fam.ofDiscordChannel
                |> Maybe.andThen (Tuple.second >> ListExtra.findOne (\c -> c.id == channelId))
                |> Maybe.withDefault (Discord.unavailableChannel channelId)
                |> (\channel -> { size = editorFontSize, channel = channel })
                |> discordChannelEl []

        LocalMessageEditor _ ->
            text "Personal Memo"


editorFontSize : Int
editorFontSize =
    scale12 1


editorResetButtonEl : String -> Element Msg
editorResetButtonEl cId =
    thinButtonEl [ alignRight, mouseOver [ BG.color oneDark.err ] ]
        { onPress = ColumnCtrl cId Column.EditorReset
        , enabled = True
        , enabledColor = oneDark.bd
        , enabledFontColor = oneDark.text
        , width = shrink
        , innerElement =
            octiconEl [ paddingEach trashcanPaddingAdjust ]
                { size = editorHeaderIconSize
                , color = oneDark.text
                , shape = Octicons.trashcan
                }
        }


textAreaInputEl : Bool -> String -> ColumnEditor -> Element Msg
textAreaInputEl isActive cId ce =
    case ce of
        DiscordMessageEditor _ opts ->
            messageInputBaseEl []
                { columnId = cId
                , placeholder = "Message"
                , labelText = "Discord Message"
                , isActive = isActive
                }
                opts

        LocalMessageEditor opts ->
            messageInputBaseEl []
                { columnId = cId
                , placeholder = "Memo"
                , labelText = "Personal Memo"
                , isActive = isActive
                }
                opts


messageInputBaseEl :
    List (Attribute Msg)
    ->
        { columnId : String
        , placeholder : String
        , labelText : String
        , isActive : Bool
        }
    -> CommonEditorOpts
    -> Element Msg
messageInputBaseEl attrs iOpts eOpts =
    let
        msgTagger =
            ColumnCtrl iOpts.columnId

        ( heightPx, fontColor, bgColor ) =
            if iOpts.isActive then
                ( bufferToHeight eOpts.buffer, oneDark.text, oneDark.note )

            else
                ( editorFontSize * 2, oneDark.note, oneDark.sub )

        bufferToHeight b =
            let
                lines =
                    List.length (String.split "\n" b)
            in
            if lines < 6 then
                editorFontSize * 8

            else
                editorFontSize * 16

        baseAttrs =
            [ height (px heightPx)
            , Font.size editorFontSize
            , Font.color fontColor
            , BG.color bgColor
            , onFocus (msgTagger (Column.EditorToggle True))
            , style "resize" "none"
            ]
    in
    multilineInputEl (baseAttrs ++ attrs)
        { onChange = msgTagger << Column.EditorInput
        , text = eOpts.buffer
        , key = String.fromInt eOpts.seq
        , placeholder = Just iOpts.placeholder
        , label = Element.Input.labelHidden iOpts.labelText
        , spellcheck = True
        , width = fill
        }


selectedFilesEl : String -> ColumnEditor -> Element Msg
selectedFilesEl cId ce =
    case ce of
        DiscordMessageEditor { file } _ ->
            case file of
                Just ( f, dataUrl ) ->
                    previewWrapperEl cId f <|
                        if String.startsWith "image/" (File.mime f) then
                            image
                                [ centerX
                                , width (shrink |> maximum filePreviewMaxWidth)
                                , height (shrink |> maximum filePreviewMaxHeight)
                                ]
                                { src = dataUrl, description = "Selected image" }

                        else
                            octiconEl [ centerX, padding otherFileIconSize ]
                                { size = otherFileIconSize
                                , color = oneDark.text
                                , shape = Octicons.file
                                }

                Nothing ->
                    none

        LocalMessageEditor _ ->
            none


filePreviewMaxHeight : Int
filePreviewMaxHeight =
    400


filePreviewMaxWidth : Int
filePreviewMaxWidth =
    columnWidth - ((columnBorderWidth + rectElementInnerPadding) * 2)


otherFileIconSize : Int
otherFileIconSize =
    scale12 3


previewWrapperEl : String -> File -> Element Msg -> Element Msg
previewWrapperEl cId f =
    el
        [ width fill
        , clip
        , BD.rounded rectElementRound
        , BG.color oneDark.sub
        , Font.size editorFontSize
        , inFront (previewOverlayEl cId f)
        ]


previewOverlayEl : String -> File -> Element Msg
previewOverlayEl cId f =
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        ]
        [ el [ width fill, alignTop ] <|
            breakP
                [ width shrink
                , padding rectElementInnerPadding
                , BD.rounded rectElementRound
                , BG.color (setAlpha 0.5 oneDark.bg)
                ]
                [ breakT <|
                    File.name f
                        ++ (" (" ++ StringExtra.punctuateNumber (File.size f) ++ " bytes)")
                ]
        , el [ width fill, alignTop ] <|
            roundButtonEl
                [ alignRight
                , BG.color (setAlpha 0.5 oneDark.bg)
                ]
                { onPress = ColumnCtrl cId Column.EditorFileDiscard
                , enabled = True
                , innerElement =
                    octiconEl [ padding rectElementInnerPadding ]
                        { size = editorHeaderIconSize
                        , color = oneDark.text
                        , shape = Octicons.x
                        }
                , innerElementSize = editorHeaderIconSize + rectElementInnerPadding * 2
                }
        ]


editorButtonsEl : Int -> Bool -> String -> ColumnEditor -> Element Msg
editorButtonsEl clientHeight isActive cId ce =
    let
        rowAttrs opts =
            [ width fill
            , spacing spacingUnit
            , visible isActive
            , Font.size editorFontSize
            ]
    in
    case ce of
        DiscordMessageEditor { file } opts ->
            let
                submittable =
                    not (String.isEmpty opts.buffer) || file /= Nothing
            in
            row (rowAttrs opts)
                [ selectFileButtonEl cId
                , submitButtonEl clientHeight cId submittable
                ]

        LocalMessageEditor opts ->
            row (rowAttrs opts)
                [ submitButtonEl clientHeight cId (not (String.isEmpty opts.buffer))
                ]


selectFileButtonEl : String -> Element Msg
selectFileButtonEl cId =
    thinButtonEl
        [ alignLeft
        , BG.color oneDark.bd
        , mouseOver [ BG.color oneDark.note ]
        ]
        { onPress = ColumnCtrl cId (Column.EditorFileRequest [ "*/*" ])
        , width = px editorButtonWidth
        , enabledColor = oneDark.main
        , enabledFontColor = oneDark.text
        , enabled = True
        , innerElement =
            octiconEl [] { size = editorFontSize, color = oneDark.text, shape = Octicons.plus }
        }


submitButtonEl : Int -> String -> Bool -> Element Msg
submitButtonEl clientHeight cId enabled =
    thinButtonEl [ alignRight ]
        { onPress = ColumnCtrl cId (Column.EditorSubmit clientHeight)
        , width = px editorButtonWidth
        , enabledColor = oneDark.succ
        , enabledFontColor = oneDark.text
        , enabled = enabled
        , innerElement = text "Submit"
        }


editorButtonWidth : Int
editorButtonWidth =
    50
