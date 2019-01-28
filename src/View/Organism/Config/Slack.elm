module View.Organism.Config.Slack exposing (Effects, Props, TeamState(..), render, styles)

import Data.Producer.Slack as Slack
import Dict exposing (Dict)
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (..)
import Html.Keyed
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { onRehydrateButtonClick : msg
    }


type alias Props =
    { teamStates : List ( TeamSnip, TeamState ) -- Should be sorted already
    }


render : Effects msg -> Props -> Html msg
render eff props =
    let
        teamStates =
            List.map (teamState eff) props.teamStates
    in
    Html.Keyed.node "div" [ flexColumn, spacingColumn5 ] <|
        teamStates


type TeamState
    = NowHydrating UserSnip
    | HydratedOnce
        { user : UserSnip
        , subbableConvs : List ConvSnip
        , subbedConvs : List ConvSnip
        }


type alias TeamSnip =
    { id : String
    , name : String
    , domain : String
    , icon : Maybe String
    }


type alias UserSnip =
    { realName : String
    , displayName : Maybe String
    , image48 : String
    }


type alias ConvSnip =
    {}


teamState : Effects msg -> ( TeamSnip, TeamState ) -> ( String, Html msg )
teamState eff ( team, ts ) =
    Tuple.pair team.id <|
        div [ flexColumn, padding5, spacingColumn5, Border.round5, Border.w1 ] <|
            case ts of
                NowHydrating user ->
                    [ teamAndUser eff.onRehydrateButtonClick False team.id team user ]

                HydratedOnce opts ->
                    [ teamAndUser eff.onRehydrateButtonClick False team.id team opts.user ]


teamAndUser : msg -> Bool -> String -> TeamSnip -> UserSnip -> Html msg
teamAndUser onRehydrateButtonClick rehydrating teamIdStr team user =
    div [ flexRow, spacingRow5 ]
        [ userNameAndAvatar user
        ]


userNameAndAvatar : UserSnip -> Html msg
userNameAndAvatar user =
    div [ flexRow, flexGrow, spacingRow5 ]
        [ img
            [ flexItem
            , Icon.rounded40
            , src user.image48
            , alt (Maybe.withDefault user.realName user.displayName)
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
