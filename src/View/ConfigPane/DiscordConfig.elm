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
    column [ width fill, spacing 5 ] <|
        [ Element.Input.text
            (disabled (shouldLockInput discord)
                [ width fill
                , padding 5
                , BG.color oneDark.note
                , BD.width 0
                ]
            )
            { onChange = TokenInput
            , text = tokenText discord
            , placeholder = Nothing
            , label = tokenLabelEl
            }
        , tokenSubmitButtonEl discord
        ]
            ++ currentStateEl discord


tokenSubmitButtonEl : Discord -> Element Msg
tokenSubmitButtonEl discord =
    Element.Input.button
        ([ alignRight
         , width shrink
         , padding 10
         , BD.rounded 5
         ]
            |> disabled (shouldLockButton discord)
            |> disabledColor (shouldLockButton discord)
        )
        { onPress = ite (shouldLockButton discord) Nothing (Just CommitToken)
        , label = text (tokenInputButtonLabel discord)
        }


shouldLockInput : Discord -> Bool
shouldLockInput discord =
    case discord of
        TokenGiven _ ->
            False

        Hydrated _ _ ->
            False

        Expired _ _ ->
            False

        _ ->
            True


shouldLockButton : Discord -> Bool
shouldLockButton discord =
    case discord of
        TokenGiven "" ->
            True

        TokenGiven _ ->
            False

        Hydrated currentInput pov ->
            -- Prohibit submitting with the same token
            currentInput == pov.token

        Expired currentInput pov ->
            -- Allow submitting with the same token in this case, triggering retry
            False

        _ ->
            True


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
        column [ spacing 5 ]
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
    row [ width fill, spacing 5 ]
        [ el [] (text "User: ")
        , el
            [ width (px 32)
            , height (px 32)
            , BD.rounded 16
            , BG.uncropped (imageUrlWithFallback (Just "32") user.discriminator user.avatar)
            ]
            none
        , text user.username
        , el [ centerY, Font.size (scale12 1), Font.color oneDark.note ] (text ("#" ++ user.discriminator))
        ]


guildsEl : Bool -> POV -> Element Msg
guildsEl rotating pov =
    row [ width fill, spacing 5 ]
        [ column [ alignTop, spacing 5 ]
            [ text "Servers: "
            , rehydrateButtonEl rotating pov
            ]
        , pov.guilds
            |> Dict.foldl (\_ guild acc -> guildIconEl guild :: acc) []
            |> wrappedRow [ width fill, spacing 5 ]
        ]


guildIconEl : Guild -> Element Msg
guildIconEl guild =
    squareIconEl 50 guild.name (Maybe.map (imageUrlNoFallback (Just "64")) guild.icon)


rehydrateButtonEl : Bool -> POV -> Element Msg
rehydrateButtonEl rotating pov =
    Element.Input.button
        (disabled rotating
            [ alignLeft
            , height fill
            , BD.rounded 30
            , BG.color oneDark.main
            ]
        )
        { onPress = ite rotating Nothing (Just Rehydrate)
        , label = octiconEl Octicons.sync
        }


subbedChannelsEl : POV -> Element Msg
subbedChannelsEl pov =
    row [ width fill, spacing 5 ]
        [ column [ alignTop, spacing 5 ] [ text "Channels: " ]
        , { data =
                pov.channels
                    |> Dict.values
                    |> List.filter (.fetchStatus >> FetchStatus.isActive)
          , columns = [ subbedChannelEl, nextFetchEl ]
          }
            |> table [ width fill, spacing 5 ]
        ]


subbedChannelEl : Column Channel Msg
subbedChannelEl =
    { header = el [ BG.color oneDark.note ] (text "Name")
    , width = fill
    , view =
        \c ->
            row [ width fill, clipX ]
                [ c.guildMaybe |> Maybe.map discordGuildSmallIconEl |> Maybe.withDefault none
                , text ("#" ++ c.name)
                ]
    }


nextFetchEl : Column Channel Msg
nextFetchEl =
    { header = el [ BG.color oneDark.note ] (text "Next Fetch")
    , width = fill
    , view =
        \c ->
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
    }
