module View.ConfigPane.DiscordConfig exposing (discordConfigEl)

import Data.ColorTheme exposing (oneDark)
import Data.Filter exposing (FilterAtom(..))
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer
import Data.Producer.Discord as Discord exposing (Channel, Discord(..), POV, User)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Extra exposing (ite)
import Html.Attributes
import Octicons
import Time
import TimeExtra
import View.Parts exposing (..)


discordConfigEl : Time.Zone -> Maybe Discord -> Element Msg
discordConfigEl tz discordMaybe =
    discordConfigBodyEl tz (Maybe.withDefault (TokenGiven "") discordMaybe)


discordConfigBodyEl : Time.Zone -> Discord -> Element Msg
discordConfigBodyEl tz discord =
    column [ width fill, spacing spacingUnit ] <|
        [ tokenInputEl discord
        , el [ alignRight ] <| tokenSubmitButtonEl discord
        ]
            ++ currentStateEl tz discord


tokenInputEl : Discord -> Element Msg
tokenInputEl discord =
    textInputEl
        { onChange = Discord.TokenInput
        , theme = oneDark
        , enabled = tokenInputAllowed discord
        , text = tokenText discord
        , placeholder = Nothing
        , label = tokenLabelEl
        }
        |> mapToRoot


tokenInputAllowed : Discord -> Bool
tokenInputAllowed discord =
    case discord of
        TokenGiven _ ->
            True

        Hydrated _ _ ->
            True

        Expired _ _ ->
            True

        _ ->
            False


mapToRoot : Element Discord.Msg -> Element Msg
mapToRoot =
    map (Producer.DiscordMsg >> ProducerCtrl)


tokenSubmitButtonEl : Discord -> Element Msg
tokenSubmitButtonEl discord =
    primaryButtonEl
        { onPress = Discord.CommitToken
        , width = shrink
        , theme = oneDark
        , enabled = tokenSubmitAllowed discord
        , innerElement = text (tokenInputButtonLabel discord)
        }
        |> mapToRoot


tokenSubmitAllowed : Discord -> Bool
tokenSubmitAllowed discord =
    case discord of
        TokenGiven "" ->
            False

        TokenGiven _ ->
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
        TokenGiven string ->
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
            , paragraph [ Font.color oneDark.note, Font.size (scale12 1) ]
                [ text "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
            ]


tokenInputButtonLabel : Discord -> String
tokenInputButtonLabel discord =
    case discord of
        TokenGiven _ ->
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


currentStateEl : Time.Zone -> Discord -> List (Element Msg)
currentStateEl tz discord =
    case discord of
        Identified newSession ->
            [ userNameAndAvatarEl newSession.user ]

        Hydrated _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl tz pov
            ]

        Rehydrating _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl True pov
            , subbedChannelsEl tz pov
            ]

        Revisit pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl tz pov
            ]

        Expired _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl tz pov
            ]

        Switching newSession pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl tz pov
            ]

        _ ->
            []


userNameAndAvatarEl : User -> Element Msg
userNameAndAvatarEl user =
    row [ width fill, spacing spacingUnit ]
        [ el [] (text "User: ")
        , el
            [ width (px 32)
            , height (px 32)
            , BD.rounded 16
            , BG.uncropped (Discord.imageUrlWithFallback (Just 32) user.discriminator user.avatar)
            ]
            none
        , text user.username
        , el [ centerY, Font.size (scale12 1), Font.color oneDark.note ] (text ("#" ++ user.discriminator))
        ]


guildsEl : Bool -> POV -> Element Msg
guildsEl rehydrating pov =
    row [ width fill, spacing spacingUnit ]
        [ column [ alignTop, spacing spacingUnit ]
            [ text "Servers: "
            , rehydrateButtonEl rehydrating pov
            ]
        , pov.guilds
            |> Dict.foldl (\_ guild acc -> discordGuildIconEl 50 guild :: acc) []
            |> wrappedRow [ width fill, spacing 5 ]
        ]


rehydrateButtonEl : Bool -> POV -> Element Msg
rehydrateButtonEl rehydrating pov =
    roundButtonEl
        { onPress = Discord.Rehydrate
        , enabled = not rehydrating
        , innerElementSize = rehydrateButtonSize
        , innerElement =
            octiconEl
                { size = rehydrateButtonSize
                , color = ite rehydrating defaultOcticonColor activeRehydrateButtonColor
                , shape = Octicons.sync
                }
        }
        |> map (Producer.DiscordMsg >> ProducerCtrl)


rehydrateButtonSize : Int
rehydrateButtonSize =
    26


activeRehydrateButtonColor : Color
activeRehydrateButtonColor =
    oneDark.prim


subbedChannelsEl : Time.Zone -> POV -> Element Msg
subbedChannelsEl tz pov =
    row [ width fill ]
        [ el [ alignTop ] <| text "Channels: "
        , Element.Keyed.column [ width fill, spacing 5, Font.size channelTableFontSize ] <|
            List.concat <|
                [ [ headerKeyEl ]
                , channelRows tz pov
                ]
        ]


channelTableFontSize : Int
channelTableFontSize =
    scale12 1


channelRows : Time.Zone -> POV -> List ( String, Element Msg )
channelRows tz pov =
    pov.channels
        |> Dict.values
        |> List.filter (not << FetchStatus.dormant << .fetchStatus)
        |> List.sortWith Discord.compareByNames
        |> List.map (channelRowKeyEl tz)


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
            [ el [ width fill ] <|
                row [ spacing 2 ]
                    [ c.guildMaybe |> Maybe.map (discordGuildIconEl 20) |> Maybe.withDefault none
                    , breakP [] [ breakT ("#" ++ c.name) ]
                    ]
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
    thinButtonEl
        { onPress = AddSimpleColumn (OfDiscordChannel c.id)
        , width = fill
        , enabledColor = oneDark.prim
        , enabledFontColor = oneDark.text
        , disabledColor = oneDark.sub
        , disabledFontColor = oneDark.note
        , enabled = FetchStatus.subscribed c.fetchStatus
        , innerElement = text "Create Column"
        }


unsubscribeButtonEl : Channel -> Element Msg
unsubscribeButtonEl c =
    roundButtonEl
        { onPress = Discord.Unsubscribe c.id
        , enabled = True -- Any channels show up in table are unsubscribable
        , innerElement =
            el [ htmlAttribute (Html.Attributes.title ("Unsubscribe #" ++ c.name)) ] <|
                octiconEl
                    { size = channelTableFontSize
                    , color = oneDark.err
                    , shape = Octicons.circleSlash
                    }
        , innerElementSize = channelTableFontSize
        }
        |> mapToRoot
