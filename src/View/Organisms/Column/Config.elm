module View.Organisms.Column.Config exposing (Effects, render)

import Html exposing (Html, button, div, p, span)
import Html.Events exposing (onClick)
import Octicons
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column as Column exposing (ColumnProps)
import View.Molecules.Icon as Icon
import View.Molecules.Source as Source exposing (Source)


type alias Effects msg =
    { onCloseButtonClick : msg
    , onColumnDeleteButtonClick : String -> msg
    , onSourceSelect : String -> Source -> msg
    , selectMsgTagger : Select.Msg msg -> msg
    }


type alias Props c =
    { selectState : Select.State
    , availableSourecs : List Source -- Expects it to be already parsed
    , column :
        ColumnProps
            { c
                | id : String
                , numItems : Int
            }
    }


render : Effects msg -> Props c -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        , Background.colorSub
        , Border.w1
        , Border.solid
        , Border.colorNote
        ]
        [ configSection
            [ span [ Image.fillSucc ] [ Image.octicon { size = xProminentSize, shape = Octicons.pulse } ]
            , t " Status"
            ]
            (status props.column)
        , configSection
            [ span [ Image.fillPrim ] [ Image.octicon { size = xProminentSize, shape = Octicons.beaker } ]
            , t " Sources"
            ]
            (sources eff props)
        , configSection
            [ span [ Image.fillErr ] [ Image.octicon { size = xProminentSize, shape = Octicons.stop } ]
            , span [ colorErr ] [ t " Danger Zone" ]
            ]
            (dangerZone eff props.column)
        , closeButton eff.onCloseButtonClick
        ]


configSection : List (Html msg) -> List (Html msg) -> Html msg
configSection headerTexts contents =
    let
        header =
            div
                [ xProminent
                , padding2
                , Border.bot1
                , Border.solid
                ]
                headerTexts

        wrappedContents =
            div
                [ flexColumn
                , padding5
                , spacingColumn5
                , Border.round5
                , Background.colorMain
                ]
                contents
    in
    div [ flexColumn, spacingColumn2 ] [ header, wrappedContents ]


status : ColumnProps { c | id : String, numItems : Int } -> List (Html msg)
status props =
    List.map (div [] << List.map t << List.intersperse " - ") <|
        [ [ "ID", props.id ]
        , [ "Stored messages", StringExtra.punctuateNumber props.numItems ]
        , [ "Pinned"
          , if props.pinned then
                "Yes"

            else
                "No"
          ]
        ]


sources : Effects msg -> Props c -> List (Html msg)
sources eff props =
    sourceSelector eff props :: []


sourceSelector : Effects msg -> Props c -> Html msg
sourceSelector eff props =
    Select.render []
        { state = props.selectState
        , msgTagger = eff.selectMsgTagger
        , id = "sourceSelect_" ++ props.column.id
        , thin = True
        , onSelect = eff.onSourceSelect props.column.id
        , selectedOption = Nothing
        , filterMatch = Nothing
        , options = List.map (\s -> ( Source.id s, s )) props.availableSourecs
        , optionHtml = Source.inlineWithIcon14
        }


dangerZone : Effects msg -> ColumnProps { c | id : String } -> List (Html msg)
dangerZone eff props =
    let
        row =
            div [ flexRow, flexCenter, spacingRow5, prominent ]
    in
    [ row
        [ div [ flexGrow ]
            [ p [] [ t "Delete this column" ]
            , p [ colorNote, minuscule ] [ t "CAUTION: Messages stored in this column will be permanently discarded!" ]
            ]
        , button
            [ flexItem
            , padding5
            , Background.colorErr
            , onClick (eff.onColumnDeleteButtonClick props.id)
            ]
            [ t "Delete" ]
        ]
    ]


closeButton : msg -> Html msg
closeButton onCloseButtonClick =
    button
        [ flexItem
        , Border.noRound
        , Background.colorSub
        , Background.hovBd
        , onClick onCloseButtonClick
        ]
        [ Image.octicon { size = xProminentSize, shape = Octicons.triangleUp }
        ]
