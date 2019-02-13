module View.Molecules.Source exposing
    ( Source(..), id, theme, headTheme
    , inline, slackInline, concatInline, horizontalBlock14
    , icon, badge10, badge14
    , styles
    )

{-| Data source Molecule.

@docs Source, id, theme, headTheme
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
-}
type Source
    = DiscordSource
        { id : String
        , name : String
        , guildName : String -- Currently DM/GroupDM are not supported
        , guildIcon : Maybe String
        }
    | SlackSource
        { id : String
        , name : String
        , teamName : String
        , teamIcon : Maybe String
        , isPrivate : Bool
        }


id : Source -> String
id source =
    case source of
        DiscordSource opts ->
            "DiscordSource_" ++ opts.id

        SlackSource opts ->
            "SlackSource_" ++ opts.id


theme : Source -> Attribute msg
theme s =
    case s of
        SlackSource _ ->
            aubergine

        DiscordSource _ ->
            oneDark


headTheme : List Source -> Attribute msg
headTheme sources =
    case sources of
        [] ->
            noAttr

        s :: _ ->
            theme s


{-| Renders a list of inline Html nodes from a Source.
-}
inline : Int -> Source -> List (Html msg)
inline octiconSize source =
    case source of
        DiscordSource { name } ->
            [ t ("#" ++ name) ]

        SlackSource opts ->
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


concatInline : Int -> List Source -> List (Html msg)
concatInline octiconSize sources =
    sources |> List.map (inline octiconSize) |> List.intersperse [ t ", " ] |> List.concat


icon : List (Attribute msg) -> Source -> Html msg
icon attrs source =
    case source of
        DiscordSource opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.guildName opts.guildIcon

        SlackSource opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.teamName opts.teamIcon


badge10 : List (Attribute msg) -> Source -> Html msg
badge10 attrs source =
    case source of
        DiscordSource _ ->
            Icon.discord10 attrs

        SlackSource _ ->
            Icon.slack10 attrs


badge14 : List (Attribute msg) -> Source -> Html msg
badge14 attrs source =
    case source of
        DiscordSource _ ->
            Icon.discord14 attrs

        SlackSource _ ->
            Icon.slack14 attrs


horizontalBlock14 : Source -> Html msg
horizontalBlock14 source =
    div [ flexRow, flexCenter, spacingRow2, clip ] <|
        [ badge14 [ flexItem ] source
        , icon [ flexItem, Icon.rounded14, regular ] source
        , div [ flexGrow, flexBasisAuto, nowrap, ellipsis ] (inline regularSize source)
        ]



-- STYLES


styles : List Style
styles =
    [ s (descOf (c inlineLockIconClass) (c "octicon")) [ ( "vertical-align", "bottom" ) ] -- Adjusting lock icons' position
    ]


inlineLockIconClass : String
inlineLockIconClass =
    "illock"
