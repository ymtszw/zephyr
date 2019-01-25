module View.Organism.Config.Discord exposing (CurrentState(..), Effects, Props, render, styles)

import Color exposing (cssRgba)
import Data.Producer.Discord as Discord
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, img, input, label, p, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Octicons
import View.Atom.Animation as Animation
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image exposing (octicon, octiconPathStyle)
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Molecule.Table as Table
import View.Style exposing (..)


type alias Effects msg =
    { onTokenInput : String -> msg
    , onTokenSubmit : msg
    , onRehydrateButtonClick : msg
    , onForceFetchButtonClick : String -> msg
    , onCreateColumnButtonClick : String -> msg
    , onUnsubscribeButtonClick : String -> msg
    }


type alias Props =
    { token : String
    , tokenSubmitButtonText : String
    , tokenSubmittable : Bool
    , currentState : CurrentState
    }


render : Effects msg -> Props -> Html msg
render eff props =
    div
        [ flexColumn
        , padding5
        , spacingColumn5
        ]
        [ currentState eff props
        , tokenForm eff props
        ]


tokenForm : Effects msg -> Props -> Html msg
tokenForm eff props =
    let
        tokenInputId =
            "discordTokenInput"
    in
    div [ flexColumn, spacingColumn2 ]
        [ label [ flexItem, sizeTitle, bold, for tokenInputId ] [ t "Token" ]
        , p [ colorNote ] [ t "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
        , p [ colorNote ]
            [ t "Tokens are stored in IndexedDB of your web browser, and only sent to 'discordapp.com'. Otherwise it "
            , strong [ bold ] [ t "never" ]
            , t " get out of your web browser."
            ]
        , input
            [ type_ "text"
            , value props.token
            , id tokenInputId
            , onInput eff.onTokenInput
            , flexItem
            , sizeHeadline
            , padding5
            , Border.round5
            ]
            []
        , button
            [ class tokenSubmitButtonClass
            , flexItem
            , sizeHeadline
            , padding10
            , Background.colorPrim
            , disabled (not props.tokenSubmittable)
            , onClick eff.onTokenSubmit
            ]
            [ t props.tokenSubmitButtonText ]
        ]


type CurrentState
    = NotIdentified
    | NowHydrating Discord.User
    | HydratedOnce
        { rehydrating : Bool
        , user : Discord.User
        , guilds : Dict String Discord.Guild
        , subbedChannels : List ChannelGlance -- Must be sorted already
        }


type alias ChannelGlance =
    { id : String
    , name : String
    , guildMaybe : Maybe Discord.Guild
    , fetching : Bool -- May include InitialFetching
    , producing : Bool -- Meaning, the channel is successfully fetched at least once
    }


currentState : Effects msg -> Props -> Html msg
currentState eff props =
    case props.currentState of
        NotIdentified ->
            none

        NowHydrating user ->
            div [] [ userNameAndAvatar eff.onRehydrateButtonClick False user ]

        HydratedOnce opts ->
            div [ flexColumn, spacingColumn5 ]
                [ userNameAndAvatar eff.onRehydrateButtonClick opts.rehydrating opts.user
                , guilds opts.guilds
                , subbedChannelTable eff opts.subbedChannels
                ]


userNameAndAvatar : msg -> Bool -> Discord.User -> Html msg
userNameAndAvatar onRehydrateButtonClick rehydrating user =
    div [ flexRow, spacingRow5 ]
        [ img
            [ class icon40Class
            , flexItem
            , src (Discord.imageUrlWithFallback (Just icon40Size) user.discriminator user.avatar)
            , alt user.username
            , Border.round5
            ]
            []
        , div [ flexGrow ]
            [ h3 [ sizeHeadline, bold ] [ t user.username ]
            , p [ colorNote ] [ t ("#" ++ user.discriminator) ]
            ]
        , rehydrateButton onRehydrateButtonClick rehydrating
        ]


rehydrateButton : msg -> Bool -> Html msg
rehydrateButton onRehydrateButtonClick rehydrating =
    Icon.octiconButton
        [ class rehydrateButtonClass
        , disabled rehydrating
        , Border.elliptic
        , Background.transparent
        , if rehydrating then
            Animation.rotating

          else
            noAttr
        ]
        { onPress = onRehydrateButtonClick
        , size = octiconButtonSize
        , shape = Octicons.sync
        }


octiconButtonSize : Int
octiconButtonSize =
    20


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
        case Maybe.map (Discord.imageUrlNoFallback (Just icon40Size)) g.icon of
            Just src_ ->
                img [ class icon40Class, Border.round5, src src_, alt g.name ] []

            Nothing ->
                Icon.abbr [ class icon40Class, Border.round5, serif, sizeTitle ] g.name


subbedChannelTable : Effects msg -> List ChannelGlance -> Html msg
subbedChannelTable eff subbedChannels =
    let
        nameCell c =
            ( [ widthFill ]
            , [ div [ flexRow, flexCenter, spacingRow5 ] [ guildIcon c, div [ flexGrow ] [ t ("#" ++ c.name) ] ] ]
            )

        guildIcon c =
            case c.guildMaybe of
                Just g ->
                    Icon.imgOrAbbr [ class channelIconClass, flexItem, Border.round2 ] g.name <|
                        Maybe.map (Discord.imageUrlNoFallback (Just channelIconSize)) g.icon

                Nothing ->
                    -- TODO DM/GroupDMs should have appropriate icons
                    none

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
        [ class fetchStatusAndforceFetchButtonClass
        , flexItem
        , flexBasisAuto
        , noPadding
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
            [ octicon { size = octiconButtonSize, shape = Octicons.arrowDown }
            ]
        ]


createColumnButton : msg -> ChannelGlance -> Html msg
createColumnButton onPress c =
    button
        [ class createColumnButtonClass
        , flexItem
        , flexGrow
        , padding2
        , Background.colorPrim
        , disabled (not c.producing)
        , onClick onPress
        ]
        [ t "Create Column" ]


unsubscribeButton : msg -> Html msg
unsubscribeButton onPress =
    Icon.octiconButton
        [ class unsubscribeButtonClass
        , flexItem
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
    [ s (c tokenSubmitButtonClass) [ ( "align-self", "flex-end" ) ]
    , s (c icon40Class) [ ( "width", px icon40Size ), ( "height", px icon40Size ), ( "flex-basis", "auto" ) ]
    , s (c rehydrateButtonClass) [ ( "align-self", "flex-start" ) ]
    , octiconPathStyle (c rehydrateButtonClass) [ ( "fill", cssRgba oneDarkTheme.prim ) ]
    , octiconPathStyle (c fetchStatusAndforceFetchButtonClass ++ ":hover") [ ( "fill", cssRgba oneDarkTheme.succ ) ]
    , s (c channelIconClass) [ ( "width", px channelIconSize ), ( "height", px channelIconSize ), ( "flex-basis", "auto" ) ]
    , octiconPathStyle (c unsubscribeButtonClass ++ ":hover") [ ( "fill", cssRgba oneDarkTheme.err ) ]
    , s (c createColumnButtonClass) [ ( "width", px createColumnButtonWidth ), ( "flex-basis", "auto" ) ]
    ]


tokenSubmitButtonClass : String
tokenSubmitButtonClass =
    "discordtokenbtn"


icon40Class : String
icon40Class =
    "discordicon40"


icon40Size : Int
icon40Size =
    40


rehydrateButtonClass : String
rehydrateButtonClass =
    "discordrehy"


channelIconClass : String
channelIconClass =
    "discordchicon"


channelIconSize : Int
channelIconSize =
    20


fetchStatusAndforceFetchButtonClass : String
fetchStatusAndforceFetchButtonClass =
    "discordfetchbtn"


createColumnButtonClass : String
createColumnButtonClass =
    "discordcreatebtn"


createColumnButtonWidth : Int
createColumnButtonWidth =
    150


unsubscribeButtonClass : String
unsubscribeButtonClass =
    "discordunsubbtn"
