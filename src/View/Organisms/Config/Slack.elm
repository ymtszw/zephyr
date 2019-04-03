module View.Organisms.Config.Slack exposing (Effects, Props, SubbableConv, SubbedConv, TeamSnip, TeamState(..), UserSnip, hydratedOnce, render)

import Data.Producer.Slack as Slack
import Html exposing (Html, div, img, p)
import Html.Attributes exposing (..)
import Html.Keyed
import StringExtra
import Url
import View.Atoms.Border as Border
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (forceBreak)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.ProducerConfig as ProducerConfig
import View.Molecules.Source as Source
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : String -> msg
    , onConvSelect : String -> String -> msg
    , onForceFetchButtonClick : String -> String -> msg
    , onCreateColumnButtonClick : String -> msg
    , onUnsubscribeButtonClick : String -> String -> msg
    , selectMsgTagger : Select.Msg msg -> msg
    }


type alias Props =
    { token : String
    , tokenSubmittable : Bool
    , teamStates : List ( TeamSnip, TeamState ) -- Should be sorted already
    , selectState : Select.State
    }


type TeamState
    = NowHydrating UserSnip
    | HydratedOnce
        { rehydrating : Bool
        , user : UserSnip
        , subbableConvs : List SubbableConv
        , subbedConvs : List SubbedConv
        }


hydratedOnce : Bool -> UserSnip -> List SubbableConv -> List SubbedConv -> TeamState
hydratedOnce rehydrating user subbableConvs subbedConvs =
    HydratedOnce
        { rehydrating = rehydrating
        , user = user
        , subbableConvs = subbableConvs
        , subbedConvs = subbedConvs
        }


type alias TeamSnip =
    { id : String
    , name : String
    , domain : String
    , image44 : Maybe String
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
            List.map (teamState eff props) props.teamStates

        tokenFormKey =
            let
                id =
                    "slackTokenInput"
            in
            ( id
            , ProducerConfig.tokenForm { onInput = eff.onTokenInput, onSubmit = eff.onTokenSubmit }
                { id = id
                , token = props.token
                , submittable = props.tokenSubmittable
                , submitButtonText = "Register"
                , apiDomain = "slack.com"
                }
            )
    in
    Html.Keyed.node "div" [ flexColumn, padding5, spacingColumn5 ] <|
        teamStates
            ++ [ tokenFormKey ]


teamState : Effects msg -> Props -> ( TeamSnip, TeamState ) -> ( String, Html msg )
teamState eff props ( team, ts ) =
    Tuple.pair team.id <|
        div [ flexColumn, padding5, spacingColumn5, Border.round5, Border.w1, Border.solid ] <|
            case ts of
                NowHydrating user ->
                    [ teamAndUser (eff.onRehydrateButtonClick team.id) True team user ]

                HydratedOnce opts ->
                    [ teamAndUser (eff.onRehydrateButtonClick team.id) opts.rehydrating team opts.user
                    , ProducerConfig.subSelect (eff.onConvSelect team.id)
                        { id = "slackConvSubscribeInput_" ++ team.id
                        , selectMsgTagger = eff.selectMsgTagger
                        , selectState = props.selectState
                        , options = opts.subbableConvs
                        , filterMatch = \f conv -> StringExtra.containsCaseIgnored f conv.name
                        , optionHtml = div [] << Source.slackInline regularSize
                        }
                    , let
                        eff_ =
                            { onCreateColumnButtonClick = eff.onCreateColumnButtonClick
                            , onForceFetchButtonClick = eff.onForceFetchButtonClick team.id
                            , onUnsubscribeButtonClick = eff.onUnsubscribeButtonClick team.id
                            }
                      in
                      ProducerConfig.subbedTable eff_
                        { items = opts.subbedConvs
                        , itemHtml = div [] << Source.slackInline regularSize
                        }
                    ]


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
        [ Icon.imgOrAbbr [ flexItem, serif, xProminent, Icon.rounded40 ] team.name team.image44
        , div [ flexGrow ] <|
            let
                teamUrl =
                    Slack.teamUrl team
            in
            [ div [ prominent, bold ] [ t team.name ]
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
        , div [ flexGrow, forceBreak ] <|
            case user.displayName of
                Just dn ->
                    [ div [ prominent, bold ] [ t dn ], p [ colorNote ] [ t user.realName ] ]

                Nothing ->
                    [ div [ prominent, bold ] [ t user.realName ] ]
        ]
