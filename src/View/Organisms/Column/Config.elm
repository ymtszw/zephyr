module View.Organisms.Column.Config exposing (Effects, render)

import Html exposing (Html, button, div, span)
import Html.Events exposing (onClick)
import Octicons
import StringExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)


type alias Effects msg =
    { onCloseButtonClick : msg
    }


render :
    Effects msg
    ->
        { c
            | id : String
            , numItems : Int
            , pinned : Bool
        }
    -> Html msg
render eff c =
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
            (status c)
        , configSection
            [ span [ Image.fillErr ] [ Image.octicon { size = configHeaderOcticonSize, shape = Octicons.stop } ]
            , t " Danger Zone"
            ]
            []
        , closeButton eff.onCloseButtonClick
        ]


configSection : List (Html msg) -> List (Html msg) -> Html msg
configSection headerTexts contents =
    let
        header =
            div
                [ sizeTitle
                , colorNote
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


status : { c | id : String, numItems : Int, pinned : Bool } -> List (Html msg)
status c =
    List.map (div [] << List.map t << List.intersperse " - ") <|
        [ [ "ID", c.id ]
        , [ "Stored messages", StringExtra.punctuateNumber c.numItems ]
        , [ "Pinned"
          , if c.pinned then
                "Yes"

            else
                "No"
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
