module View.Organism.Config.Slack exposing (Effects, Props, TeamState(..), render, styles)

import Data.Producer.Slack as Slack
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (..)
import Html.Keyed
import Url
import View.Atom.Border as Border
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Molecule.ProducerTokenForm as ProducerTokenForm
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : msg
    }


type alias Props =
    { token : String
    , tokenSubmittable : Bool
    , teamStates : List ( TeamSnip, TeamState ) -- Should be sorted already
    }


type TeamState
    = NowHydrating UserSnip
    | HydratedOnce
        { rehydrating : Bool
        , user : UserSnip
        , subbableConvs : List SubbableConv
        , subbedConvs : List SubbedConv
        }


type alias TeamSnip =
    { id : String
    , name : String
    , domain : String
    , image48 : Maybe String
    }


type alias UserSnip =
    { realName : String
    , displayName : Maybe String
    , image48 : String
    }


type alias SubbableConv =
    { id : String
    , name : String
    , isPrivate : Bool
    }


type alias SubbedConv =
    { id : String
    , name : String
    , isPrivate : Bool
    , fetching : Bool -- May include InitialFetching
    , producing : Bool -- Meaning, the conversation is successfully fetched at least once
    }


render : Effects msg -> Props -> Html msg
render eff props =
    let
        teamStates =
            List.map (teamState eff) props.teamStates

        tokenFormKey =
            let
                id =
                    "slackTokenInput"
            in
            ( id
            , ProducerTokenForm.render { onInput = eff.onTokenInput, onSubmit = eff.onTokenSubmit }
                { id = id
                , token = props.token
                , submittable = props.tokenSubmittable
                , submitButtonText = "Register"
                , apiDomain = "slack.com"
                }
            )
    in
    Html.Keyed.node "div" [ flexColumn, spacingColumn5 ] <|
        teamStates
            ++ [ tokenFormKey ]


teamState : Effects msg -> ( TeamSnip, TeamState ) -> ( String, Html msg )
teamState eff ( team, ts ) =
    Tuple.pair team.id <|
        div [ flexColumn, padding5, spacingColumn5, Border.round5, Border.w1, Border.solid ] <|
            case ts of
                NowHydrating user ->
                    [ teamAndUser eff.onRehydrateButtonClick True team user ]

                HydratedOnce opts ->
                    [ teamAndUser eff.onRehydrateButtonClick opts.rehydrating team opts.user ]


teamAndUser : msg -> Bool -> TeamSnip -> UserSnip -> Html msg
teamAndUser onRehydrateButtonClick rehydrating team user =
    div [ flexRow, spacingRow5 ]
        [ teamNameAndIcon team
        , userNameAndAvatar user
        , Icon.rehydrateButton onRehydrateButtonClick rehydrating
        ]


teamNameAndIcon : TeamSnip -> Html msg
teamNameAndIcon team =
    div [ flexRow, flexGrow, spacingRow5 ]
        [ Icon.imgOrAbbr [ flexItem, serif, sizeTitle, Icon.rounded40 ] team.name team.image48
        , div [ flexGrow ] <|
            let
                teamUrl =
                    Slack.teamUrl team
            in
            [ h3 [ sizeHeadline, bold ] [ t team.name ]
            , ntLink [] { url = Url.toString teamUrl, children = [ t teamUrl.host ] }
            ]
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
