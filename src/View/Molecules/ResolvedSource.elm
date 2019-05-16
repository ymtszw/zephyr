module View.Molecules.ResolvedSource exposing
    ( ResolvedSource(..), discordChannel, slackConvo, desiredIconSize, id, theme, headTheme
    , inline, slackInline, concatInline, horizontalBlock14
    , icon, badge10, badge14
    , styles
    )

{-| Resolved (joined) data source Molecule.

From Data.Column.Source and other stored data, ResolvedSource must be constructed on Pages.

@docs ResolvedSource, discordChannel, slackConvo, desiredIconSize, id, theme, headTheme
@docs inline, slackInline, concatInline, horizontalBlock14
@docs icon, badge10, badge14
@docs styles

-}

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Octicons
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip, ellipsis, nowrap)
import View.Atoms.Theme exposing (aubergine, oneDark)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


{-| Representation of data sources for columns.

In current usages, the maximum desired icon size is 40px (desiredIconSize).

-}
type ResolvedSource
    = DiscordChannel
        { id : String
        , name : String
        , guildName : String -- Currently DM/GroupDM are not supported
        , guildIcon : Maybe String
        }
    | SlackConvo
        { id : String
        , name : String
        , isPrivate : Bool
        , teamId : String
        , teamName : String
        , teamIcon : Maybe String
        }


discordChannel : String -> String -> String -> Maybe String -> ResolvedSource
discordChannel id_ name guildName guildIcon =
    DiscordChannel { id = id_, name = name, guildName = guildName, guildIcon = guildIcon }


slackConvo : String -> String -> Bool -> String -> String -> Maybe String -> ResolvedSource
slackConvo id_ name isPrivate teamId teamName teamIcon =
    SlackConvo { id = id_, name = name, isPrivate = isPrivate, teamId = teamId, teamName = teamName, teamIcon = teamIcon }


desiredIconSize : Int
desiredIconSize =
    40


id : ResolvedSource -> String
id source =
    case source of
        DiscordChannel opts ->
            "DiscordSource_" ++ opts.id

        SlackConvo opts ->
            "SlackSource_" ++ opts.id


theme : ResolvedSource -> Attribute msg
theme s =
    case s of
        SlackConvo _ ->
            aubergine

        DiscordChannel _ ->
            oneDark


headTheme : List ResolvedSource -> Attribute msg
headTheme sources =
    case sources of
        [] ->
            noAttr

        s :: _ ->
            theme s


{-| Renders a list of inline Html nodes from a ResolvedSource.
-}
inline : Int -> ResolvedSource -> List (Html msg)
inline octiconSize source =
    case source of
        DiscordChannel { name } ->
            [ t ("#" ++ name) ]

        SlackConvo opts ->
            slackInline octiconSize opts


slackInline : Int -> { s | name : String, isPrivate : Bool } -> List (Html msg)
slackInline octiconSize { name, isPrivate } =
    [ if isPrivate then
        span [ class inlineLockIconClass, Image.fillText ]
            [ Image.octicon { size = octiconSize, shape = Octicons.lock } ]

      else
        t "#"
    , t name
    ]


concatInline : Int -> List ResolvedSource -> List (Html msg)
concatInline octiconSize sources =
    sources |> List.map (inline octiconSize) |> List.intersperse [ t ", " ] |> List.concat


icon : List (Attribute msg) -> ResolvedSource -> Html msg
icon attrs source =
    case source of
        DiscordChannel opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.guildName opts.guildIcon

        SlackConvo opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.teamName opts.teamIcon


badge10 : List (Attribute msg) -> ResolvedSource -> Html msg
badge10 attrs source =
    case source of
        DiscordChannel _ ->
            Icon.discord10 attrs

        SlackConvo _ ->
            Icon.slack10 attrs


badge14 : List (Attribute msg) -> ResolvedSource -> Html msg
badge14 attrs source =
    case source of
        DiscordChannel _ ->
            Icon.discord14 attrs

        SlackConvo _ ->
            Icon.slack14 attrs


horizontalBlock14 : ResolvedSource -> Html msg
horizontalBlock14 source =
    div [ flexRow, flexCenter, spacingRow2, clip ] <|
        [ badge14 [ flexItem ] source
        , icon [ flexItem, Icon.rounded14, regular ] source
        , div [ flexGrow, flexBasisAuto, flexShrink, nowrap, ellipsis ] (inline regularSize source)
        ]



-- STYLES


styles : List Style
styles =
    [ s (descOf (c inlineLockIconClass) (c "octicon")) [ ( "vertical-align", "bottom" ) ] -- Adjusting lock icons' position
    ]


inlineLockIconClass : String
inlineLockIconClass =
    "illock"
