module View.Organisms.Column.Config exposing (Effects, render)

import Html exposing (Html, div)
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
            [ div [ Image.fillSucc ] [ Image.octicon { size = configHeaderOcticonSize, shape = Octicons.pulse } ]
            , div [] [ t " Status" ]
            ]
            (status c)
        ]


configSection : List (Html msg) -> List (Html msg) -> Html msg
configSection headerTexts contents =
    let
        header =
            div
                [ flexRow
                , flexCenter
                , padding2
                , spacingRow5
                , sizeTitle
                , colorNote
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
