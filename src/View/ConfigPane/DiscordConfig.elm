module View.ConfigPane.DiscordConfig exposing (discordConfigEl)

import Data.ColorTheme exposing (oneDark)
import Data.Producer.Discord as Discord exposing (..)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Extra exposing (..)
import Iso8601
import Octicons
import View.Parts exposing (..)


discordConfigEl : Maybe Discord -> Element Msg
discordConfigEl discordMaybe =
    case discordMaybe of
        Just discord ->
            tokenFormEl discord

        Nothing ->
            tokenFormEl (TokenGiven "")


tokenFormEl : Discord -> Element Msg
tokenFormEl discord =
    column [ width fill, spacing spacingUnit ] <|
        [ textInputEl
            { onChange = TokenInput
            , theme = oneDark
            , enabled = tokenInputAllowed discord
            , text = tokenText discord
            , placeholder = Nothing
            , label = tokenLabelEl
            }
        , el [ alignRight ] <| tokenSubmitButtonEl discord
        ]
            ++ currentStateEl discord


tokenSubmitButtonEl : Discord -> Element Msg
tokenSubmitButtonEl discord =
    primaryButtonEl
        { onPress = CommitToken
        , width = shrink
        , theme = oneDark
        , enabled = tokenSubmitAllowed discord
        , innerElement = text (tokenInputButtonLabel discord)
        }


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

        ChannelScanning pov ->
            pov.token

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

        ChannelScanning pov ->
            let
                ( done, total ) =
                    ( Dict.foldl (\_ c acc -> ite (not (initializing c)) (acc + 1) acc) 0 pov.channels
                    , Dict.size pov.channels
                    )
            in
            "Scanning Channels... (" ++ String.fromInt done ++ "/" ++ String.fromInt total ++ ")"

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


currentStateEl : Discord -> List (Element Msg)
currentStateEl discord =
    case discord of
        Identified newSession ->
            [ userNameAndAvatarEl newSession.user ]

        ChannelScanning pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            ]

        Hydrated _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl pov
            ]

        Rehydrating _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl True pov
            , subbedChannelsEl pov
            ]

        Revisit pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl pov
            ]

        Expired _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl pov
            ]

        Switching newSession pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            , subbedChannelsEl pov
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
            , BG.uncropped (imageUrlWithFallback (Just 32) user.discriminator user.avatar)
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
        { onPress = Rehydrate
        , enabled = not rehydrating
        , innerElementSize = rehydrateButtonSize
        , innerElement =
            octiconEl
                { size = rehydrateButtonSize
                , color = ite rehydrating defaultOcticonColor activeRehydrateButtonColor
                , shape = Octicons.sync
                }
        }


rehydrateButtonSize : Int
rehydrateButtonSize =
    26


activeRehydrateButtonColor : Color
activeRehydrateButtonColor =
    oneDark.prim


subbedChannelsEl : POV -> Element Msg
subbedChannelsEl pov =
    row [ width fill ]
        [ el [ alignTop ] <| text "Channels: "
        , Element.Keyed.column [ width fill, spacing 5, Font.size (scale12 1) ] <|
            [ headerKeyEl
            ]
                ++ channelRows pov
        ]


channelRows : POV -> List ( String, Element Msg )
channelRows pov =
    pov.channels
        |> Dict.values
        |> List.filter (FetchStatus.isActive << .fetchStatus)
        |> List.sortWith Discord.compareByFetchStatus
        |> List.map channelRowKeyEl


headerKeyEl : ( String, Element Msg )
headerKeyEl =
    Tuple.pair "ChannelHeader" <|
        row [ width fill, spacing 2 ]
            [ el [ width fill, BG.color oneDark.note ] <| text "Name"
            , el [ width fill, BG.color oneDark.note ] <| text "Next Fetch"
            ]


channelRowKeyEl : Channel -> ( String, Element Msg )
channelRowKeyEl c =
    Tuple.pair c.id <|
        row [ width fill, spacing 2, clipX ]
            [ el [ width fill ] <|
                row [ spacing 2 ]
                    [ c.guildMaybe |> Maybe.map (discordGuildIconEl 20) |> Maybe.withDefault none
                    , breakP [] [ breakT ("#" ++ c.name) ]
                    ]
            , el [ width fill ] <|
                text <|
                    case c.fetchStatus of
                        Waiting ->
                            "Soon"

                        ResumeFetching ->
                            "Fetching..."

                        NextFetchAt posix _ ->
                            Iso8601.fromTime posix

                        Fetching _ _ ->
                            "Fetching..."

                        _ ->
                            "Not active"
            ]
