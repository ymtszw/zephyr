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
import Html.Attributes exposing (placeholder)
import ListExtra
import Octicons
import SelectArray exposing (SelectArray)
import View.Parts exposing (..)
import View.Select as Select


newMessageEditorEl : Select.State -> FilterAtomMaterial -> Column.Column -> Element Msg
newMessageEditorEl ss fam c =
    column
        [ width fill
        , height shrink
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BD.color oneDark.bd
        , BD.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        ]
        [ row [ spacing spacingUnit, centerY ]
            [ octiconEl [] { size = editorFontSize, color = defaultOcticonColor, shape = Octicons.pencil }
            , editorSelectEl ss fam c
            ]
        , textAreaInputEl c.id (SelectArray.selected c.editors)
        ]


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
        , id = c.id ++ "_newMessageEditorSelect"
        , theme = oneDark
        , onSelect = onEditorSelect c.id selectedIndex
        , selectedOption = Just ( selectedIndex, SelectArray.selected c.editors )
        , options = indexedEditors
        , optionEl = editorSelectOptionEl fam
        }


editorSelectWidth : Int
editorSelectWidth =
    150


onEditorSelect : String -> Int -> ( Int, ColumnEditor ) -> Msg
onEditorSelect cId selectedIndex ( index, _ ) =
    if index == selectedIndex then
        NoOp

    else
        ColumnCtrl cId (Column.SelectEditor index)


editorSelectOptionEl : FilterAtomMaterial -> ( Int, ColumnEditor ) -> Element Msg
editorSelectOptionEl fam ( _, ce ) =
    case ce of
        DiscordMessageEditor channelId _ ->
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


textAreaInputEl : String -> ColumnEditor -> Element Msg
textAreaInputEl cId ce =
    case ce of
        DiscordMessageEditor _ opts ->
            localMessageInputEl (ColumnCtrl cId) opts

        LocalMessageEditor opts ->
            localMessageInputEl (ColumnCtrl cId) opts


localMessageInputEl : (Column.Msg -> Msg) -> CommonEditorOpts -> Element Msg
localMessageInputEl msgTagger opts =
    let
        ( heightPx, fontColor, bgColor ) =
            if opts.focused || not (String.isEmpty opts.buffer) then
                ( bufferToHeight opts.buffer, oneDark.text, oneDark.note )

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
    in
    Element.Input.multiline
        [ height (px heightPx)
        , padding rectElementInnerPadding
        , Font.size editorFontSize
        , Font.color fontColor
        , BG.color bgColor
        , onFocus (msgTagger (Column.EditorFocus True))
        , onLoseFocus (msgTagger (Column.EditorFocus False))
        , BD.width 0
        , style "resize" "none"
        , style "transition" "height 0.2s 0.1s,background-color 0.3s,color 0.3s"
        , htmlAttribute (placeholder "Memo")
        ]
        { onChange = msgTagger << Column.EditorInput
        , text = opts.buffer
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Personal Memo"
        , spellcheck = True
        }
