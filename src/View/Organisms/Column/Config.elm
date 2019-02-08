module View.Organisms.Column.Config exposing (Effects, render)

import Html exposing (Html, button, div, p, span)
import Html.Events exposing (onClick)
import Octicons
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column exposing (ColumnProps)


type alias Effects msg =
    { onCloseButtonClick : msg
    , onColumnDeleteButtonClick : String -> msg
    }


type alias Props c =
    ColumnProps
        { c
            | id : String
            , numItems : Int
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
            [ span [ Image.fillSucc ] [ Image.octicon { size = configHeaderOcticonSize, shape = Octicons.pulse } ]
            , t " Status"
            ]
            (status props)
        , configSection
            [ span [ Image.fillErr ] [ Image.octicon { size = configHeaderOcticonSize, shape = Octicons.stop } ]
            , span [ colorErr ] [ t " Danger Zone" ]
            ]
            (dangerZone eff props)
        , closeButton eff.onCloseButtonClick
        ]


configSection : List (Html msg) -> List (Html msg) -> Html msg
configSection headerTexts contents =
    let
        header =
            div
                [ sizeTitle
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


configHeaderOcticonSize : Int
configHeaderOcticonSize =
    18


status : Props c -> List (Html msg)
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


dangerZone : Effects msg -> Props c -> List (Html msg)
dangerZone eff props =
    let
        row =
            div [ flexRow, flexCenter, spacingRow5, sizeHeadline ]
    in
    [ row
        [ div [ flexGrow ]
            [ p [] [ t "Delete this column" ]
            , p [ colorNote, sizeDetail ] [ t "CAUTION: Messages stored in this column will be permanently discarded!" ]
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
        [ Image.octicon { size = closeTriangleSize, shape = Octicons.triangleUp }
        ]


closeTriangleSize : Int
closeTriangleSize =
    24
