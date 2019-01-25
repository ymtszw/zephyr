module View.Organism.Config.Slack exposing (Effects, Props, render, styles)

import Data.Producer.Slack as Slack
import Dict exposing (Dict)
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (..)
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Style exposing (..)


type alias Effects msg =
    { onRehydrateButtonClick : msg
    }


type alias Props =
    { teamStates : List ( String, TeamState )
    }


render : Effects msg -> Props -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        []


type TeamState
    = NowHydrating { team : Slack.Team, user : UserGlance }


type alias UserGlance =
    { realName : String
    , displayName : Maybe String
    , image48 : String
    }


teamState : Effects msg -> ( String, TeamState ) -> ( String, Html msg )
teamState eff ( teamIdStr, ts ) =
    Tuple.pair teamIdStr <|
        div [ flexColumn, padding5, spacingColumn5, Border.round5, Border.w1 ] <|
            case ts of
                NowHydrating opts ->
                    [ teamAndUser eff.onRehydrateButtonClick False teamIdStr opts.team opts.user ]


teamAndUser : msg -> Bool -> String -> Slack.Team -> UserGlance -> Html msg
teamAndUser onRehydrateButtonClick rehydrating teamIdStr team user =
    div [ flexRow, spacingRow5 ]
        [ userNameAndAvatar user
        ]


userNameAndAvatar : UserGlance -> Html msg
userNameAndAvatar user =
    div [ flexRow, flexGrow, spacingRow5 ]
        [ img
            [ flexItem
            , src user.image48
            , alt (Maybe.withDefault user.realName user.displayName)
            , Border.round5
            ]
            []
        , div [ flexGrow ] <|
            case user.displayName of
                Just dn ->
                    [ h3 [ sizeHeadline, bold ] [ t dn ], p [ colorNote ] [ t user.realName ] ]

                Nothing ->
                    [ h3 [ sizeHeadline, bold ] [ t user.realName ] ]
        ]



-- STYLES


styles : List Style
styles =
    []
