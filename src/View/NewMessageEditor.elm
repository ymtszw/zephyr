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


newMessageEditorEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
newMessageEditorEl ss fam c =
    let
        selectedEditor =
            SelectArray.selected c.editors
    in
    column
        [ width fill
        , alignTop
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BD.color oneDark.bd
        , BD.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        ]
        [ row [ width fill, spacing spacingUnit, centerY, visible c.editorActive ]
            [ octiconEl [] { size = editorHeaderIconSize, color = defaultOcticonColor, shape = Octicons.pencil }
            , editorSelectEl ss fam c
            , editorResetButtonEl c.id
            , editorDismissButtonEl c.id
            ]
        , textAreaInputEl c selectedEditor
        , selectedFilesEl c selectedEditor
        , editorButtonsEl c.editorActive c.id selectedEditor
        ]


editorHeaderIconSize : Int
editorHeaderIconSize =
    scale12 2


editorSelectEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
editorSelectEl ss fam c =
    let
        indexedEditors =
            SelectArray.indexedMap (\{ index, e } -> ( String.fromInt index, ( index, e ) )) c.editors

        selectedIndex =
            SelectArray.selectedIndex c.editors
    in
    Select.select [ width (px editorSelectWidth), Font.size editorFontSize ]
        { state = ss
        , msgTagger = SelectCtrl
        , id = editorSelectId c.id
        , theme = oneDark
        , thin = True
        , onSelect = onEditorSelect c.id selectedIndex
        , selectedOption = Just ( selectedIndex, SelectArray.selected c.editors )
        , filterMatch = Nothing
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
                |> Maybe.map (\c -> discordChannelEl [] { size = editorFontSize, channel = c })
                |> Maybe.withDefault (text channelId)

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
        , enabledColor = oneDark.main
        , enabledFontColor = oneDark.text
        , width = shrink
        , innerElement =
            octiconEl [ paddingEach trashcanPaddingAdjust ]
                { size = editorHeaderIconSize
                , color = oneDark.text
                , shape = Octicons.trashcan
                }
        }


editorDismissButtonEl : String -> Element Msg
editorDismissButtonEl cId =
    thinButtonEl [ alignRight, mouseOver [ BG.color oneDark.note ] ]
        { onPress = ColumnCtrl cId (Column.EditorToggle False)
        , enabled = True
        , enabledColor = oneDark.main
        , enabledFontColor = oneDark.text
        , width = shrink
        , innerElement =
            octiconEl []
                { size = editorHeaderIconSize
                , color = oneDark.text
                , shape = Octicons.x
                }
        }


textAreaInputEl : Column.Column -> ColumnEditor -> Element Msg
textAreaInputEl c ce =
    case ce of
        DiscordMessageEditor _ opts ->
            messageInputBaseEl []
                { columnId = c.id
                , seq = c.editorSeq
                , placeholder = "Message"
                , labelText = "Discord Message"
                , isActive = c.editorActive
                }
                opts

        LocalMessageEditor opts ->
            messageInputBaseEl []
                { columnId = c.id
                , seq = c.editorSeq
                , placeholder = "Memo"
                , labelText = "Personal Memo"
                , isActive = c.editorActive
                }
                opts


messageInputBaseEl :
    List (Attribute Msg)
    ->
        { columnId : String
        , seq : Int
        , placeholder : String
        , labelText : String
        , isActive : Bool
        }
    -> CommonEditorOpts
    -> Element Msg
messageInputBaseEl attrs opts { buffer } =
    let
        msgTagger =
            ColumnCtrl opts.columnId

        ( heightPx, fontColor, bgColor ) =
            if opts.isActive then
                ( bufferToHeight buffer, oneDark.text, oneDark.note )

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
            , if opts.isActive then
                noneAttr

              else
                onFocus (msgTagger (Column.EditorToggle True))
            , style "resize" "none"
            ]
    in
    multilineInputEl (baseAttrs ++ attrs)
        { onChange = msgTagger << Column.EditorInput
        , text = buffer
        , key = String.fromInt opts.seq
        , placeholder = Just opts.placeholder
        , label = Element.Input.labelHidden opts.labelText
        , spellcheck = True
        , width = fill
        }


selectedFilesEl : Column.Column -> ColumnEditor -> Element Msg
selectedFilesEl c ce =
    case ( c.editorActive, ce ) of
        ( True, DiscordMessageEditor { file } _ ) ->
            case file of
                Just ( f, dataUrl ) ->
                    previewWrapperEl c.id f <|
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

        _ ->
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
    let
        wrapInP t =
            breakP
                [ width shrink
                , padding rectElementInnerPadding
                , BD.rounded rectElementRound
                , BG.color (setAlpha 0.5 oneDark.bg)
                ]
                [ t ]
    in
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        ]
        [ column [ width (fillPortion 2), alignTop ] <|
            List.map wrapInP
                [ el [ Font.bold ] <| breakT <| File.name f
                , breakT <| File.mime f ++ " (" ++ StringExtra.punctuateNumber (File.size f) ++ " Bytes)"
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


editorButtonsEl : Bool -> String -> ColumnEditor -> Element Msg
editorButtonsEl isActive cId ce =
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
                , submitButtonEl cId submittable
                ]

        LocalMessageEditor opts ->
            row (rowAttrs opts)
                [ submitButtonEl cId (not (String.isEmpty opts.buffer))
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


submitButtonEl : String -> Bool -> Element Msg
submitButtonEl cId enabled =
    thinButtonEl [ alignRight ]
        { onPress = ColumnCtrl cId Column.EditorSubmit
        , width = px editorButtonWidth
        , enabledColor = oneDark.succ
        , enabledFontColor = oneDark.text
        , enabled = enabled
        , innerElement = text "Submit"
        }


editorButtonWidth : Int
editorButtonWidth =
    50
