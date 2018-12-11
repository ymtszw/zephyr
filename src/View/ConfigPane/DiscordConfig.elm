module View.ConfigPane.DiscordConfig exposing (discordConfigEl)

import Data.ColorTheme exposing (oneDark)
import Data.Filter exposing (FilterAtom(..))
import Data.Model exposing (ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord exposing (Channel, Discord(..), POV, User)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Data.ProducerRegistry as ProducerRegistry
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy2)
import Html.Attributes
import Octicons
import Time
import TimeExtra
import View.Parts exposing (..)
import View.Select as Select exposing (select)


discordConfigEl : ViewState -> Discord -> Element Msg
discordConfigEl vs discord =
    let
        currentStates =
            currentStateEl vs discord

        tokenForm =
            [ lazy2 tokenInputEl (tokenInputAllowed discord) (tokenText discord)
            , lazy2 tokenSubmitButtonEl (tokenSubmitAllowed discord) (tokenSubmitButtonText discord)
            ]
    in
    column
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        ]
        (currentStates ++ tokenForm)


tokenInputEl : Bool -> String -> Element Msg
tokenInputEl enabled text =
    textInputEl
        { onChange = Discord.TokenInput
        , theme = oneDark
        , enabled = enabled
        , text = text
        , placeholder = Nothing
        , label = tokenLabelEl
        }
        |> mapToRoot


tokenInputAllowed : Discord -> Bool
tokenInputAllowed discord =
    case discord of
        TokenWritable _ ->
            True

        Hydrated _ _ ->
            True

        Expired _ _ ->
            True

        _ ->
            False


mapToRoot : Element Discord.Msg -> Element Msg
mapToRoot =
    map (ProducerRegistry.DiscordMsg >> ProducerCtrl)


tokenSubmitButtonEl : Bool -> String -> Element Msg
tokenSubmitButtonEl enabled text_ =
    primaryButtonEl [ alignRight ]
        { onPress = Discord.TokenCommit
        , width = shrink
        , theme = oneDark
        , enabled = enabled
        , innerElement = text text_
        }
        |> mapToRoot


tokenSubmitAllowed : Discord -> Bool
tokenSubmitAllowed discord =
    case discord of
        TokenWritable "" ->
            False

        TokenWritable _ ->
            True

        Hydrated currentInput pov ->
            -- Prohibit submitting with the same token
            currentInput /= pov.token

        Expired currentInput pov ->
            -- Allow submitting with the same token in this case, triggering retry
            True

        _ ->
            False


tokenText : Discord -> String
tokenText discord =
    case discord of
        TokenWritable string ->
            string

        TokenReady string ->
            string

        Identified newSession ->
            newSession.token

        Hydrated token _ ->
            token

        Rehydrating token _ ->
            token

        Switching newSession _ ->
            newSession.token

        Revisit pov ->
            pov.token

        Expired token _ ->
            token


tokenLabelEl : Element.Input.Label msg
tokenLabelEl =
    Element.Input.labelAbove [] <|
        column [ spacing spacingUnit ]
            [ el [] (text "Token")
            , paragraph [ Font.color oneDark.note, Font.size smallFontSize ]
                [ text "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
            , paragraph [ Font.color oneDark.note, Font.size smallFontSize ]
                [ text "Tokens are stored in IndexedDB of your web browser, and only sent to 'discordapp.com'. Otherwise it "
                , el [ Font.bold ] (text "never")
                , text " get out of your web browser."
                ]
            ]


smallFontSize : Int
smallFontSize =
    scale12 1


tokenSubmitButtonText : Discord -> String
tokenSubmitButtonText discord =
    case discord of
        TokenWritable _ ->
            "Register"

        TokenReady _ ->
            "Waiting..."

        Identified _ ->
            "Fetching data..."

        Hydrated token _ ->
            if token == "" then
                "Unregister"

            else
                "Change Token"

        Rehydrating _ _ ->
            "Fetching data..."

        Switching _ _ ->
            "Switching Identity"

        Revisit _ ->
            "Reloading..."

        Expired _ _ ->
            "Change Token"


currentStateEl : ViewState -> Discord -> List (Element Msg)
currentStateEl vs discord =
    case discord of
        Identified newSession ->
            [ userNameAndAvatarEl False newSession.user ]

        Hydrated _ pov ->
            [ userNameAndAvatarEl False pov.user
            , guildsEl pov
            , subbedChannelsEl vs pov
            ]

        Rehydrating _ pov ->
            [ userNameAndAvatarEl True pov.user
            , guildsEl pov
            , subbedChannelsEl vs pov
            ]

        Revisit pov ->
            [ userNameAndAvatarEl False pov.user
            , guildsEl pov
            , subbedChannelsEl vs pov
            ]

        Expired _ pov ->
            [ userNameAndAvatarEl False pov.user
            , guildsEl pov
            , subbedChannelsEl vs pov
            ]

        Switching newSession pov ->
            [ userNameAndAvatarEl False pov.user
            , guildsEl pov
            , subbedChannelsEl vs pov
            ]

        _ ->
            []


userNameAndAvatarEl : Bool -> User -> Element Msg
userNameAndAvatarEl rehydrating user =
    row [ width fill, spacing spacingUnit ]
        [ el [] (text "User: ")
        , el
            [ width (px userAvatarSize)
            , height (px userAvatarSize)
            , BD.rounded (userAvatarSize // 2 + 1)
            , BG.uncropped (Discord.imageUrlWithFallback (Just userAvatarSize) user.discriminator user.avatar)
            ]
            none
        , breakT user.username
        , el [ Font.size smallFontSize, Font.color oneDark.note ] (breakT ("#" ++ user.discriminator))
        , rehydrateButtonEl rehydrating
        ]


userAvatarSize : Int
userAvatarSize =
    32


rehydrateButtonEl : Bool -> Element Msg
rehydrateButtonEl rehydrating =
    roundButtonEl [ alignRight ]
        { onPress = Discord.Rehydrate
        , enabled = not rehydrating
        , innerElementSize = rehydrateButtonSize
        , innerElement =
            octiconEl [ rotating rehydrating ]
                { size = rehydrateButtonSize
                , color =
                    if rehydrating then
                        defaultOcticonColor

                    else
                        activeRehydrateButtonColor
                , shape = Octicons.sync
                }
        }
        |> map (ProducerRegistry.DiscordMsg >> ProducerCtrl)


rehydrateButtonSize : Int
rehydrateButtonSize =
    26


activeRehydrateButtonColor : Color
activeRehydrateButtonColor =
    oneDark.prim


guildsEl : POV -> Element Msg
guildsEl pov =
    row [ width fill, spacing spacingUnit ]
        [ el [ alignTop ] <| text "Servers: "
        , pov.guilds
            |> Dict.foldl (\_ guild acc -> discordGuildIconEl [] guildIconSize guild :: acc) []
            |> wrappedRow [ width fill, spacing spacingUnit ]
        ]


guildIconSize : Int
guildIconSize =
    50


subbedChannelsEl : ViewState -> POV -> Element Msg
subbedChannelsEl vs pov =
    row [ width fill ]
        [ el [ alignTop ] <| text "Channels: "
        , Element.Keyed.column [ width fill, spacing 5, Font.size channelTableFontSize ] <|
            List.concat <|
                [ [ headerKeyEl ]
                , channelRows vs pov
                ]
        ]


channelTableFontSize : Int
channelTableFontSize =
    smallFontSize


channelRows : ViewState -> POV -> List ( String, Element Msg )
channelRows vs pov =
    let
        ( notSubbed, subbed ) =
            Dict.values pov.channels
                |> List.sortWith Discord.compareByNames
                |> List.partition (.fetchStatus >> FetchStatus.dormant)
    in
    subbed
        |> List.map (channelRowKeyEl vs.timezone)
        |> (::) (subscribeRowKeyEl vs.selectState notSubbed)


headerKeyEl : ( String, Element Msg )
headerKeyEl =
    Tuple.pair "ChannelHeader" <|
        row [ width fill, spacing 2 ]
            [ el [ width fill, BG.color oneDark.note ] <| text "Name"
            , el [ width fill, BG.color oneDark.note ] <| text "Fetch Status"
            , el [ width fill, BG.color oneDark.note ] <| text "Action"
            ]


channelRowKeyEl : Time.Zone -> Channel -> ( String, Element Msg )
channelRowKeyEl tz c =
    Tuple.pair c.id <|
        row [ width fill, spacing 2, clipX ]
            [ discordChannelEl [ width fill ] { size = smallFontSize, channel = c }
            , el [ width fill ] <| fetchStatusTextEl tz c.fetchStatus
            , row [ width fill, spacing spacingUnit ]
                [ createColumnButtonEl c
                , unsubscribeButtonEl c
                ]
            ]


fetchStatusTextEl : Time.Zone -> FetchStatus -> Element Msg
fetchStatusTextEl tz fs =
    breakT <|
        case fs of
            NextFetchAt posix _ ->
                "Next: " ++ TimeExtra.local tz posix

            Fetching _ _ ->
                "Fetching..."

            Waiting ->
                "Checking availability..."

            InitialFetching _ ->
                "Checking availability..."

            Available ->
                "Not subscribed"


createColumnButtonEl : Channel -> Element Msg
createColumnButtonEl c =
    thinButtonEl []
        { onPress = AddSimpleColumn (OfDiscordChannel c.id)
        , width = fill
        , enabledColor = oneDark.prim
        , enabledFontColor = oneDark.text
        , enabled = FetchStatus.subscribed c.fetchStatus
        , innerElement = text "Create Column"
        }


unsubscribeButtonEl : Channel -> Element Msg
unsubscribeButtonEl c =
    roundButtonEl []
        { onPress = Discord.Unsubscribe c.id
        , enabled = True -- Any channels show up in table are unsubscribable
        , innerElement =
            octiconEl [ htmlAttribute (Html.Attributes.title ("Unsubscribe #" ++ c.name)) ]
                { size = channelTableFontSize
                , color = oneDark.err
                , shape = Octicons.circleSlash
                }
        , innerElementSize = channelTableFontSize
        }
        |> mapToRoot


subscribeRowKeyEl : Select.State -> List Channel -> ( String, Element Msg )
subscribeRowKeyEl selectState notSubbed =
    [ select [ width (fillPortion 1), alignLeft ]
        { state = selectState
        , id = channelSelectId
        , theme = oneDark
        , thin = True
        , onSelect = ProducerCtrl << ProducerRegistry.DiscordMsg << Discord.Subscribe << .id
        , selectedOption = Nothing
        , options = List.map (\c -> ( c.id, c )) notSubbed
        , optionEl = \c -> discordChannelEl [] { size = channelTableFontSize, channel = c }
        }
    , el [ width (fillPortion 2) ] none
    ]
        |> row [ width fill ]
        |> Tuple.pair channelSelectId


channelSelectId : String
channelSelectId =
    "discordChannelSelect"


channelSelectWidth : Int
channelSelectWidth =
    200
