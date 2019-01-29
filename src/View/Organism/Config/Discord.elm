module View.Organism.Config.Discord exposing (CurrentState(..), Effects, Props, render, styles)

import Data.Producer.Discord as Discord
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, img, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Octicons
import View.Atom.Animation as Animation
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image as Image
import View.Atom.Input.Select as Select
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Molecule.ProducerConfig as ProducerConfig
import View.Molecule.Table as Table
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : msg
    , onChannelSelected : String -> msg
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
                , subscribeChannelInput eff.onChannelSelected props opts.subbableChannels
                , subbedChannelTable eff opts.subbedChannels
                ]


userNameAndAvatar : msg -> Bool -> Discord.User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ flexItem
            , Icon.rounded40
            , src (Discord.imageUrlWithFallback Icon.size40 user.discriminator user.avatar)
            , alt user.username
            ]
            []
        , div [ flexGrow ]
            [ h3 [ sizeHeadline, bold ] [ t user.username ]
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
        case Maybe.map (Discord.imageUrlNoFallback Icon.size40) g.icon of
            Just src_ ->
                img [ Icon.rounded40, src src_, alt g.name ] []

            Nothing ->
                Icon.abbr [ Icon.rounded40, serif, sizeTitle ] g.name


subscribeChannelInput : (String -> msg) -> Props msg -> List SubbableChannel -> Html msg
subscribeChannelInput onSelect props subbableChannels =
    div [ flexRow, flexCenter, spacingRow5 ]
        [ div [ sizeHeadline ] [ t "Subscribe:" ]
        , Select.render [ class subscribeChannelInputClass, flexBasisAuto ]
            { state = props.selectState
            , msgTagger = props.selectMsgTagger
            , id = "discordChannelSubscribeInput"
            , thin = True
            , onSelect = .id >> onSelect
            , selectedOption = Nothing
            , filterMatch = Just Discord.channelFilter
            , options = List.map (\c -> ( c.id, c )) subbableChannels
            , optionHtml = channelSummary
            }
        ]


channelSummary : { c | name : String, guildMaybe : Maybe Discord.Guild } -> Html msg
channelSummary c =
    let
        guildIcon =
            case c.guildMaybe of
                Just g ->
                    Icon.imgOrAbbr [ flexItem, Icon.rounded20 ] g.name <|
                        Maybe.map (Discord.imageUrlNoFallback Icon.size20) g.icon

                Nothing ->
                    -- TODO DM/GroupDMs should have appropriate icons
                    none
    in
    div [ flexRow, flexCenter, spacingRow5 ] [ guildIcon, div [ flexGrow ] [ t ("#" ++ c.name) ] ]


subbedChannelTable : Effects msg -> List SubbedChannel -> Html msg
subbedChannelTable eff subbedChannels =
    let
        nameCell c =
            ( [ widthFill ], [ channelSummary c ] )

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
        , data = subbedChannels
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


createColumnButton : msg -> SubbedChannel -> Html msg
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
    [ s (c subscribeChannelInputClass) [ ( "width", px subscribeChannelInputWidth ) ]
    , s (c createColumnButtonClass) [ ( "width", px createColumnButtonWidth ) ]
    ]


subscribeChannelInputClass : String
subscribeChannelInputClass =
    "discordsubchinput"


subscribeChannelInputWidth : Int
subscribeChannelInputWidth =
    250


createColumnButtonClass : String
createColumnButtonClass =
    "discordcreatebtn"


createColumnButtonWidth : Int
createColumnButtonWidth =
    150
