module View.Molecule.ProducerConfig exposing (styles, subSelect, subbedTable, tokenForm)

import Html exposing (Html, button, div, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Octicons
import View.Atom.Animation as Animation
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image as Image
import View.Atom.Input.Select as Select
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Molecule.Table as Table
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


subbedTable :
    { a
        | onCreateColumnButtonClick : String -> msg
        , onForceFetchButtonClick : String -> msg
        , onUnsubscribeButtonClick : String -> msg
    }
    ->
        { items : List { x | id : String, fetching : Bool, producing : Bool }
        , itemHtml : { x | id : String, fetching : Bool, producing : Bool } -> Html msg
        }
    -> Html msg
subbedTable eff props =
    let
        nameCell c =
            ( [ widthFill ], [ props.itemHtml c ] )

        actionCell c =
            ( []
            , [ div [ flexRow, flexCenter, spacingRow2 ]
                    [ fetchStatusAndforceFetchButton (eff.onForceFetchButtonClick c.id) c.fetching
                    , createColumnButton (eff.onCreateColumnButtonClick c.id) c
                    , unsubscribeButton (eff.onUnsubscribeButtonClick c.id)
                    ]
              ]
            )
    in
    Table.render []
        { columns =
            [ { header = "Name", cell = nameCell }
            , { header = "Action", cell = actionCell }
            ]
        , rowKey = .id
        , data = props.items
        }


fetchStatusAndforceFetchButton : msg -> Bool -> Html msg
fetchStatusAndforceFetchButton onPress fetching =
    button
        [ flexItem
        , flexBasisAuto
        , noPadding
        , Image.hovSucc
        , Border.round2
        , Background.transparent
        , onClick onPress
        ]
        [ div
            [ -- Animate inner contents, not the button itself, to keep the clickable area stable
              if fetching then
                Animation.slideDown

              else
                noAttr
            ]
            [ Image.octicon { size = octiconButtonSize, shape = Octicons.arrowDown }
            ]
        ]


octiconButtonSize : Int
octiconButtonSize =
    20


createColumnButton : msg -> { a | producing : Bool } -> Html msg
createColumnButton onPress c =
    button
        [ class createColumnButtonClass
        , flexItem
        , flexGrow
        , flexBasisAuto
        , padding2
        , Background.colorPrim
        , disabled (not c.producing)
        , onClick onPress
        ]
        [ t "Create Column" ]


unsubscribeButton : msg -> Html msg
unsubscribeButton onPress =
    Icon.octiconButton
        [ flexItem
        , Image.hovErr
        , Border.elliptic
        , Background.transparent
        ]
        { onPress = onPress
        , size = octiconButtonSize
        , shape = Octicons.circleSlash
        }



-- STYLES


styles : List Style
styles =
    [ s (c subSelectClass) [ ( "width", px subSelectWidth ) ]
    , s (c createColumnButtonClass) [ ( "width", px createColumnButtonWidth ) ]
    ]


subSelectClass : String
subSelectClass =
    "psubslct"


subSelectWidth : Int
subSelectWidth =
    250


createColumnButtonClass : String
createColumnButtonClass =
    "pccolbtn"


createColumnButtonWidth : Int
createColumnButtonWidth =
    150
