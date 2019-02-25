module View.NewMessageEditor exposing (newMessageEditorEl)

import Data.ColorTheme exposing (ColorTheme)
import Data.Column as Column
import Data.ColumnEditor exposing (ColumnEditor(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events exposing (onFocus)
import Element.Font as Font
import Element.Input
import File exposing (File)
import List.Extra
import Octicons
import SelectArray
import StringExtra
import View.Parts exposing (..)
import View.Select as Select


newMessageEditorEl : ColorTheme -> Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
newMessageEditorEl theme ss fam c =
    let
        selectedEditor =
            SelectArray.selected c.editors
    in
    column
        [ width fill
        , alignTop
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BD.color theme.bd
        , BD.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        , fluidContainer
        ]
        [ row [ width fill, spacing spacingUnit, centerY, visible c.editorActive ]
            [ octiconEl [] { size = editorHeaderIconSize, color = defaultOcticonColor, shape = Octicons.pencil }
            , editorSelectEl theme ss fam c
            , editorResetButtonEl theme c.id
            , editorDismissButtonEl theme c.id
            ]
        , textAreaInputEl theme c selectedEditor
        , selectedFilesEl theme c selectedEditor
        , editorButtonsEl theme c.editorActive c.id selectedEditor
        ]


editorHeaderIconSize : Int
editorHeaderIconSize =
    scale12 2


editorSelectEl : ColorTheme -> Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
editorSelectEl theme ss fam c =
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
        , theme = theme
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
        DiscordMessageEditor { channelId } ->
            fam.ofDiscordChannel
                |> Maybe.andThen (Tuple.second >> List.Extra.find (\c -> c.id == channelId))
                |> Maybe.map (\c -> discordChannelEl [] { size = editorFontSize, channel = c })
                |> Maybe.withDefault (text channelId)

        LocalMessageEditor _ ->
            text "Personal Memo"


editorFontSize : Int
editorFontSize =
    scale12 1


editorResetButtonEl : ColorTheme -> String -> Element Msg
editorResetButtonEl theme cId =
    thinButtonEl [ alignRight, mouseOver [ BG.color theme.err ] ]
        { onPress = ColumnCtrl cId Column.EditorReset
        , enabled = True
        , enabledColor = theme.main
        , enabledFontColor = theme.text
        , width = shrink
        , innerElement =
            octiconEl [ paddingEach trashcanPaddingAdjust ]
                { size = editorHeaderIconSize
                , color = theme.text
                , shape = Octicons.trashcan
                }
        }


editorDismissButtonEl : ColorTheme -> String -> Element Msg
editorDismissButtonEl theme cId =
    thinButtonEl [ alignRight, mouseOver [ BG.color theme.note ] ]
        { onPress = ColumnCtrl cId (Column.EditorToggle False)
        , enabled = True
        , enabledColor = theme.main
        , enabledFontColor = theme.text
        , width = shrink
        , innerElement =
            octiconEl []
                { size = editorHeaderIconSize
                , color = theme.text
                , shape = Octicons.x
                }
        }


textAreaInputEl : ColorTheme -> Column.Column -> ColumnEditor -> Element Msg
textAreaInputEl theme c ce =
    case ce of
        DiscordMessageEditor opts ->
            messageInputBaseEl []
                { columnId = c.id
                , seq = c.editorSeq
                , placeholder = "Message"
                , labelText = "Discord Message"
                , isActive = c.editorActive
                , theme = theme
                }
                opts.buffer

        LocalMessageEditor buffer ->
            messageInputBaseEl []
                { columnId = c.id
                , seq = c.editorSeq
                , placeholder = "Memo"
                , labelText = "Personal Memo"
                , isActive = c.editorActive
                , theme = theme
                }
                buffer


messageInputBaseEl :
    List (Attribute Msg)
    ->
        { columnId : String
        , seq : Int
        , placeholder : String
        , labelText : String
        , isActive : Bool
        , theme : ColorTheme
        }
    -> String
    -> Element Msg
messageInputBaseEl attrs opts buffer =
    let
        msgTagger =
            ColumnCtrl opts.columnId

        ( heightPx, fontColor, bgColor ) =
            if opts.isActive then
                ( bufferToHeight buffer, opts.theme.text, opts.theme.note )

            else
                ( editorFontSize * 2, opts.theme.note, opts.theme.sub )

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
            [ style "height" (String.fromInt heightPx ++ "px") -- Improve performance by avoiding style recalculation
            , Font.size editorFontSize
            , Font.color fontColor
            , BG.color bgColor
            , if opts.isActive then
                noneAttr

              else
                onFocus (msgTagger (Column.EditorToggle True))
            , style "resize" "none"
            , strictContainer
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


selectedFilesEl : ColorTheme -> Column.Column -> ColumnEditor -> Element Msg
selectedFilesEl theme c ce =
    case ( c.editorActive, ce ) of
        ( True, DiscordMessageEditor { file } ) ->
            case file of
                Just ( f, dataUrl ) ->
                    previewWrapperEl theme c.id f <|
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
                                , color = theme.text
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


previewWrapperEl : ColorTheme -> String -> File -> Element Msg -> Element Msg
previewWrapperEl theme cId f =
    el
        [ width fill
        , clip
        , BD.rounded rectElementRound
        , BG.color theme.sub
        , Font.size editorFontSize
        , inFront (previewOverlayEl theme cId f)
        ]


previewOverlayEl : ColorTheme -> String -> File -> Element Msg
previewOverlayEl theme cId f =
    let
        wrapInP t =
            breakP
                [ width shrink
                , padding rectElementInnerPadding
                , BD.rounded rectElementRound
                , BG.color (setAlpha 0.5 theme.bg)
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
                , BG.color (setAlpha 0.5 theme.bg)
                ]
                { onPress = ColumnCtrl cId Column.EditorFileDiscard
                , enabled = True
                , innerElement =
                    octiconEl [ padding rectElementInnerPadding ]
                        { size = editorHeaderIconSize
                        , color = theme.text
                        , shape = Octicons.x
                        }
                , innerElementSize = editorHeaderIconSize + rectElementInnerPadding * 2
                }
        ]


editorButtonsEl : ColorTheme -> Bool -> String -> ColumnEditor -> Element Msg
editorButtonsEl theme isActive cId ce =
    let
        rowAttrs =
            [ width fill
            , spacing spacingUnit
            , visible isActive
            , Font.size editorFontSize
            ]
    in
    case ce of
        DiscordMessageEditor opts ->
            let
                submittable =
                    not (String.isEmpty opts.buffer) || opts.file /= Nothing
            in
            row rowAttrs
                [ selectFileButtonEl theme cId
                , submitButtonEl theme cId submittable
                ]

        LocalMessageEditor buffer ->
            row rowAttrs
                [ submitButtonEl theme cId (not (String.isEmpty buffer))
                ]


selectFileButtonEl : ColorTheme -> String -> Element Msg
selectFileButtonEl theme cId =
    thinButtonEl
        [ alignLeft
        , BG.color theme.bd
        , mouseOver [ BG.color theme.note ]
        ]
        { onPress = ColumnCtrl cId (Column.EditorFileRequest [ "*/*" ])
        , width = px editorButtonWidth
        , enabledColor = theme.main
        , enabledFontColor = theme.text
        , enabled = True
        , innerElement =
            octiconEl [] { size = editorFontSize, color = theme.text, shape = Octicons.plus }
        }


submitButtonEl : ColorTheme -> String -> Bool -> Element Msg
submitButtonEl theme cId enabled =
    thinButtonEl [ alignRight ]
        { onPress = ColumnCtrl cId Column.EditorSubmit
        , width = px editorButtonWidth
        , enabledColor = theme.succ
        , enabledFontColor = theme.text
        , enabled = enabled
        , innerElement = text "Submit"
        }


editorButtonWidth : Int
editorButtonWidth =
    50
