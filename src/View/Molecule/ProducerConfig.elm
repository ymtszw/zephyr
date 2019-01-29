module View.Molecule.ProducerConfig exposing (styles, subSelect, tokenForm)

import Html exposing (Html, button, div, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Input.Select as Select
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Style exposing (..)


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


subSelect :
    (String -> msg)
    ->
        { id : String
        , selectMsgTagger : Select.Msg msg -> msg
        , selectState : Select.State
        , options : List { x | id : String }
        , filterMatch : String -> { x | id : String } -> Bool
        , optionHtml : { x | id : String } -> Html msg
        }
    -> Html msg
subSelect onSelect props =
    div [ flexRow, flexCenter, spacingRow5 ]
        [ div [ sizeHeadline ] [ t "Subscribe:" ]
        , Select.render [ class subSelectClass, flexBasisAuto ]
            { state = props.selectState
            , msgTagger = props.selectMsgTagger
            , id = props.id
            , thin = True
            , onSelect = .id >> onSelect
            , selectedOption = Nothing
            , filterMatch = Just props.filterMatch
            , options = List.map (\c -> ( c.id, c )) props.options
            , optionHtml = props.optionHtml
            }
        ]



-- STYLES


styles : List Style
styles =
    [ s (c subSelectClass) [ ( "width", px subSelectWidth ) ]
    ]


subSelectClass : String
subSelectClass =
    "psubslct"


subSelectWidth : Int
subSelectWidth =
    250
