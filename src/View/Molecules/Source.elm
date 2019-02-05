module View.Molecules.Source exposing (Source(..), badgedIcon20, badgedIcon30, concatInline, inline)

import Html exposing (Html, span)
import Octicons
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon


type Source
    = DiscordSource { channelName : String, guildIcon : Maybe String }
    | SlackSource { convName : String, teamIcon : Maybe String, isPrivate : Bool }


inline : Int -> Source -> List (Html msg)
inline octiconSize source =
    case source of
        DiscordSource { channelName } ->
            [ t ("#" ++ channelName) ]

        SlackSource { convName, isPrivate } ->
            [ if isPrivate then
                span [ Image.fillText ] [ Image.octicon { size = octiconSize, shape = Octicons.lock } ]

              else
                t "#"
            , t convName
            ]


concatInline : Int -> List Source -> List (Html msg)
concatInline octiconSize sources =
    sources |> List.map (inline octiconSize) |> List.intersperse [ t ", " ] |> List.concat


badgedIcon20 : Source -> Html msg
badgedIcon20 source =
    let
        ( bottomRight, content ) =
            case source of
                DiscordSource opts ->
                    ( Icon.discordBadge10
                    , Icon.imgOrAbbr [ Icon.rounded20, serif ] opts.channelName opts.guildIcon
                    )

                SlackSource opts ->
                    ( Icon.slackBadge10
                    , Icon.imgOrAbbr [ Icon.rounded20, serif ] opts.convName opts.teamIcon
                    )
    in
    withBadge [{- No outset -}]
        { topRight = Nothing
        , bottomRight = Just bottomRight
        , content = content
        }


badgedIcon30 : Source -> Html msg
badgedIcon30 source =
    let
        ( bottomRight, content ) =
            case source of
                DiscordSource opts ->
                    ( Icon.discordBadge14
                    , Icon.imgOrAbbr [ Icon.rounded30, serif, sizeTitle ] opts.channelName opts.guildIcon
                    )

                SlackSource opts ->
                    ( Icon.slackBadge14
                    , Icon.imgOrAbbr [ Icon.rounded30, serif, sizeTitle ] opts.convName opts.teamIcon
                    )
    in
    withBadge [ badgeOutset ]
        { topRight = Nothing
        , bottomRight = Just bottomRight
        , content = content
        }
