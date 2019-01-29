module View.Molecule.ProducerConfig exposing (tokenForm)

import Html exposing (Html, button, div, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)


tokenForm :
    { onInput : String -> msg
    , onSubmit : msg
    }
    ->
        { id : String
        , token : String
        , submittable : Bool
        , submitButtonText : String
        , apiDomain : String
        }
    -> Html msg
tokenForm eff props =
    div [ flexColumn, spacingColumn2 ]
        [ label [ flexItem, sizeTitle, bold, for props.id ] [ t "Token" ]
        , p [ colorNote ]
            [ t ("Tokens are stored in IndexedDB of your web browser, and only sent to '" ++ props.apiDomain ++ "'. Otherwise it ")
            , strong [ bold ] [ t "NEVER" ]
            , t " get out of your web browser."
            ]
        , input
            [ type_ "text"
            , value props.token
            , id props.id
            , onInput eff.onInput
            , flexItem
            , sizeHeadline
            , padding5
            , Border.round5
            ]
            []
        , button
            [ flexItem
            , alignEnd
            , sizeHeadline
            , padding10
            , Background.colorPrim
            , Border.round5
            , disabled (not props.submittable)
            , onClick eff.onSubmit
            ]
            [ t props.submitButtonText ]
        ]
