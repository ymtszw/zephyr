module View.Organisms.Config.Discord exposing (CurrentState(..), Effects, Props, render)

import Data.Producer.Discord as Discord
import Dict exposing (Dict)
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (..)
import Html.Keyed
import View.Atoms.Input.Select as Select
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.ProducerConfig as ProducerConfig
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : msg
    , onChannelSelect : String -> msg
    , onForceFetchButtonClick : String -> msg
    , onCreateColumnButtonClick : String -> msg
    , onUnsubscribeButtonClick : String -> msg
    }


type alias Props msg =
    { token : String
    , tokenSubmitButtonText : String
    , tokenSubmittable : Bool
    , currentState : CurrentState
    , selectMsgTagger : Select.Msg msg -> msg
    , selectState : Select.State
    }


render : Effects msg -> Props msg -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        [ currentState eff props
        , ProducerConfig.tokenForm { onInput = eff.onTokenInput, onSubmit = eff.onTokenSubmit }
            { id = "discordTokenInput"
            , token = props.token
            , submittable = props.tokenSubmittable
            , submitButtonText = props.tokenSubmitButtonText
            , apiDomain = "api.discordapp.com"
            }
        ]


type CurrentState
    = NotIdentified
    | NowHydrating Discord.User
    | HydratedOnce
        { rehydrating : Bool
        , user : Discord.User
        , guilds : Dict String Discord.Guild
        , subbableChannels : List SubbableChannel
        , subbedChannels : List SubbedChannel -- Must be sorted already
        }


type alias SubbableChannel =
    { id : String
    , name : String
    , guildMaybe : Maybe Discord.Guild
    }


type alias SubbedChannel =
    { id : String
    , name : String
    , guildMaybe : Maybe Discord.Guild
    , fetching : Bool -- May include InitialFetching
    , producing : Bool -- Meaning, the channel is successfully fetched at least once
    }


currentState : Effects msg -> Props msg -> Html msg
currentState eff props =
    case props.currentState of
        NotIdentified ->
            none

        NowHydrating user ->
            div [] [ userNameAndAvatar eff.onRehydrateButtonClick True user ]

        HydratedOnce opts ->
            div [ flexColumn, spacingColumn5 ]
                [ userNameAndAvatar eff.onRehydrateButtonClick opts.rehydrating opts.user
                , guilds opts.guilds
                , ProducerConfig.subSelect eff.onChannelSelect
                    { id = "discordChannelSubscribeInput"
                    , selectMsgTagger = props.selectMsgTagger
                    , selectState = props.selectState
                    , options = opts.subbableChannels
                    , filterMatch = Discord.channelFilter
                    , optionHtml = channelSummary
                    }
                , ProducerConfig.subbedTable eff { items = opts.subbedChannels, itemHtml = channelSummary }
                ]


userNameAndAvatar : msg -> Bool -> Discord.User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ flexItem
            , Icon.rounded40
            , src (Icon.discordImageUrlWithFallback40 user.discriminator user.avatar)
            , alt user.username
            ]
            []
        , div [ flexGrow ]
            [ h3 [ headline, bold ] [ t user.username ]
            , p [ colorNote ] [ t ("#" ++ user.discriminator) ]
            ]
        , Icon.rehydrateButton onRehydrateButtonClick rehydrating
        ]


guilds : Dict String Discord.Guild -> Html msg
guilds guilds_ =
    Html.Keyed.node "div" [ flexRow, flexWrap, spacingWrapped5 ] <|
        if Dict.isEmpty guilds_ then
            [ ( "discordGuildEmpty", p [ colorNote, flexGrow ] [ t "(No Servers)" ] ) ]

        else
            Dict.foldr (\_ g a -> guildIconKey g :: a) [] guilds_


guildIconKey : Discord.Guild -> ( String, Html msg )
guildIconKey g =
    Tuple.pair g.id <|
        case Maybe.map Icon.discordImageUrl40 g.icon of
            Just src_ ->
                img [ Icon.rounded40, src src_, alt g.name ] []

            Nothing ->
                Icon.abbr [ Icon.rounded40, serif, sizeTitle ] g.name


channelSummary : { c | name : String, guildMaybe : Maybe Discord.Guild } -> Html msg
channelSummary c =
    let
        guildIcon =
            case c.guildMaybe of
                Just g ->
                    Icon.imgOrAbbr [ flexItem, Icon.rounded20 ] g.name <|
                        Maybe.map Icon.discordImageUrl20 g.icon

                Nothing ->
                    -- TODO DM/GroupDMs should have appropriate icons
                    none
    in
    div [ flexRow, flexCenter, spacingRow5 ] [ guildIcon, div [ flexGrow ] [ t ("#" ++ c.name) ] ]
