module View.Organisms.Config.Discord exposing (CurrentState(..), Effects, Props, SubbableChannel, SubbedChannel, hydratedOnce, render)

import AssocList as Dict exposing (Dict)
import Data.Producer.Discord.Channel as Channel
import Data.Producer.Discord.Guild as Guild exposing (Guild)
import Data.Producer.Discord.User as User exposing (User)
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (..)
import Html.Keyed
import Id
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
    , onChannelSelect : Channel.Id -> msg
    , onForceFetchButtonClick : Channel.Id -> msg
    , onCreateColumnButtonClick : Channel.Id -> msg
    , onUnsubscribeButtonClick : Channel.Id -> msg
    , selectMsgTagger : Select.Msg msg -> msg
    }


type alias Props =
    { token : String
    , tokenSubmitButtonText : String
    , tokenSubmittable : Bool
    , currentState : CurrentState
    , selectState : Select.State
    }


render : Effects msg -> Props -> Html msg
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
    | NowHydrating User
    | HydratedOnce
        { rehydrating : Bool
        , user : User
        , guilds : Dict Guild.Id Guild
        , subbableChannels : List SubbableChannel
        , subbedChannels : List SubbedChannel -- Must be sorted already
        }


hydratedOnce :
    Bool
    -> User
    -> Dict Guild.Id Guild
    -> List SubbableChannel
    -> List SubbedChannel
    -> CurrentState
hydratedOnce rehydrating user guilds_ subbableChannels subbedChannels =
    HydratedOnce
        { rehydrating = rehydrating
        , user = user
        , guilds = guilds_
        , subbableChannels = subbableChannels
        , subbedChannels = subbedChannels
        }


type alias SubbableChannel =
    { id : Channel.Id
    , name : String
    , guildMaybe : Maybe Guild
    }


type alias SubbedChannel =
    { id : Channel.Id
    , name : String
    , guildMaybe : Maybe Guild
    , fetching : Bool -- May include InitialFetching
    , producing : Bool -- Meaning, the channel is successfully fetched at least once
    }


currentState : Effects msg -> Props -> Html msg
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
                , ProducerConfig.subSelect
                    { onSelect = eff.onChannelSelect
                    , selectMsgTagger = eff.selectMsgTagger
                    }
                    { id = "discordChannelSubscribeInput"
                    , selectState = props.selectState
                    , options = opts.subbableChannels
                    , filterMatch = Channel.filterByStringShared
                    , optionHtml = channelSummary
                    }
                , ProducerConfig.subbedTable eff { items = opts.subbedChannels, itemHtml = channelSummary }
                ]


userNameAndAvatar : msg -> Bool -> User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ flexItem
            , Icon.rounded40
            , src (Icon.discordUserAvatarUrl40 user)
            , alt (User.getUsername user)
            ]
            []
        , div [ flexGrow ]
            [ h3 [ prominent, bold ] [ t (User.getUsername user) ]
            , p [ colorNote ] [ t ("#" ++ User.getDiscriminator user) ]
            ]
        , Icon.rehydrateButton onRehydrateButtonClick rehydrating
        ]


guilds : Dict Guild.Id Guild -> Html msg
guilds guilds_ =
    Html.Keyed.node "div" [ flexRow, flexWrap, spacingWrapped5 ] <|
        if Dict.isEmpty guilds_ then
            [ ( "discordGuildEmpty", p [ colorNote, flexGrow ] [ t "(No Servers)" ] ) ]

        else
            Dict.foldr (\_ g a -> guildIconKey g :: a) [] guilds_


guildIconKey : Guild -> ( String, Html msg )
guildIconKey g =
    Tuple.pair (Id.to (Guild.getId g)) <|
        Icon.imgOrAbbr [ flexItem, serif, xProminent, Icon.rounded40 ] (Guild.getName g) <|
            Icon.discordGuildIconUrl40 g


channelSummary : { c | name : String, guildMaybe : Maybe Guild } -> Html msg
channelSummary c =
    let
        guildIcon =
            case c.guildMaybe of
                Just g ->
                    Icon.imgOrAbbr [ flexItem, Icon.rounded20 ] (Guild.getName g) <|
                        Icon.discordGuildIconUrl20 g

                Nothing ->
                    -- TODO DM/GroupDMs should have appropriate icons
                    none
    in
    div [ flexRow, flexCenter, spacingRow5 ] [ guildIcon, div [ flexGrow ] [ t ("#" ++ c.name) ] ]
