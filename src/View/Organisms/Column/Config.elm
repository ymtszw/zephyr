module View.Organisms.Column.Config exposing (Effects, render, selectId)

import Data.Column as Column
import Html exposing (Html, button, div, p, span)
import Html.Events exposing (onClick)
import Html.Keyed
import Id
import Octicons
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column exposing (ColumnProps)
import View.Molecules.ResolvedSource as ResolvedSource exposing (ResolvedSource(..))


type alias Effects msg =
    { onCloseButtonClick : Column.Id -> msg
    , onColumnDeleteButtonClick : Column.Id -> msg
    , onSourceSelect : Column.Id -> ResolvedSource -> msg
    , selectMsgTagger : Select.Msg msg -> msg
    , onRemoveSourceButtonClick : Column.Id -> ResolvedSource -> msg
    }


type alias Props c =
    { selectState : Select.State
    , availableSourecs : List ResolvedSource -- Expects it to be already parsed
    , column :
        ColumnProps
            { c
                | id : Column.Id
                , numItems : Int
            }
    }


render : Effects msg -> Props c -> Html msg
render eff props =
    div
        [ flexColumn
        , flexBasisAuto
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
        , closeButton (eff.onCloseButtonClick props.column.id)
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
                , flexBasisAuto
                , padding5
                , spacingColumn5
                , Border.round5
                , Background.colorMain
                ]
                contents
    in
    div [ flexColumn, flexBasisAuto, spacingColumn2 ] [ header, wrappedContents ]


status : ColumnProps { c | id : Column.Id, numItems : Int } -> List (Html msg)
status props =
    List.map (div [] << List.map t << List.intersperse " - ") <|
        [ [ "Stored messages", StringExtra.punctuateNumber props.numItems ]
        , [ "Pinned"
          , if props.pinned then
                "Yes"

            else
                "No"
          ]
        ]


sources : Effects msg -> Props c -> List (Html msg)
sources eff props =
    [ sourceSelector eff props
    , Html.Keyed.node "div" [ flexColumn, flexBasisAuto ] <|
        List.map (selectedSource eff props.column.id) props.column.sources
    ]


sourceSelector : Effects msg -> Props c -> Html msg
sourceSelector eff props =
    Select.render []
        { state = props.selectState
        , msgTagger = eff.selectMsgTagger
        , id = selectId props.column.id
        , thin = True
        , onSelect = eff.onSourceSelect props.column.id
        , selectedOption = Nothing
        , filterMatch = Just sourceFilterMatch
        , options = List.map (\s -> ( ResolvedSource.id s, s )) props.availableSourecs
        , optionHtml = ResolvedSource.horizontalBlock14
        }


selectId : Column.Id -> String
selectId columnId =
    "sourceSelect_" ++ Id.to columnId


sourceFilterMatch : String -> ResolvedSource -> Bool
sourceFilterMatch query source =
    case source of
        DiscordChannel opts ->
            StringExtra.containsCaseIgnored query opts.name
                || StringExtra.containsCaseIgnored query opts.guildName

        SlackConvo opts ->
            StringExtra.containsCaseIgnored query opts.name
                || StringExtra.containsCaseIgnored query opts.teamName


selectedSource : Effects msg -> Column.Id -> ResolvedSource -> ( String, Html msg )
selectedSource eff id s =
    Tuple.pair (Id.to id ++ ResolvedSource.id s) <|
        div [ flexRow, flexCenter ]
            [ div [ flexGrow, flexBasisAuto, flexShrink, clip, padding5, Background.colorSub, Border.leftRound5 ]
                [ ResolvedSource.horizontalBlock14 s ]
            , button
                [ flexItem
                , padding5
                , Border.rightRound5
                , Background.colorSub
                , Background.hovBd
                , Image.hovErr
                , onClick (eff.onRemoveSourceButtonClick id s)
                ]
                [ Image.octicon { size = regularSize, shape = Octicons.x } ]
            ]


dangerZone : Effects msg -> ColumnProps { c | id : Column.Id } -> List (Html msg)
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
