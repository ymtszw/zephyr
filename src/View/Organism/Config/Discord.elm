module View.Organism.Config.Discord exposing (Effects, Props, render, styles)

import Html exposing (Html, button, div, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Style exposing (Style, c, s)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    }


type alias Props =
    { token : String
    , tokenSubmitButtonText : String
    , tokenSubmittable : Bool
    }


render : Effects msg -> Props -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        [ tokenForm eff props
        ]


tokenForm : Effects msg -> Props -> Html msg
tokenForm eff props =
    let
        tokenInputId =
            "discordTokenInput"
    in
    div [ flexColumn, spacingColumn2 ]
        [ label [ flexItem, sizeTitle, bold, for tokenInputId ] [ t "Token" ]
        , p [ colorNote ] [ t "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
        , p [ colorNote ]
            [ t "Tokens are stored in IndexedDB of your web browser, and only sent to 'discordapp.com'. Otherwise it "
            , strong [ bold ] [ t "never" ]
            , t " get out of your web browser."
            ]
        , input
            [ type_ "text"
            , value props.token
            , id tokenInputId
            , onInput eff.onTokenInput
            , flexItem
            , sizeHeadline
            , padding5
            , Border.round5
            ]
            []
        , button
            [ class tokenSubmitButtonClass
            , flexItem
            , sizeHeadline
            , padding10
            , Background.colorPrim
            , disabled (not props.tokenSubmittable)
            , onClick eff.onTokenSubmit
            ]
            [ t props.tokenSubmitButtonText ]
        ]


styles : List Style
styles =
    [ s (c tokenSubmitButtonClass) [ ( "align-self", "flex-end" ) ]
    ]


tokenSubmitButtonClass : String
tokenSubmitButtonClass =
    "discordtokenbtn"
