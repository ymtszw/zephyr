module View.NewMessageEditor exposing (newMessageEditorEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnEditor exposing (ColumnEditor(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import ListExtra
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
        , BD.color frameColor
        , BD.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
        ]
        [ editorSelectEl ss fam c
        ]


frameColor : Color
frameColor =
    oneDark.bd


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
        DiscordMessageEditor { channelId } ->
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
