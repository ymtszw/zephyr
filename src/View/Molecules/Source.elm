module View.Molecules.Source exposing
    ( Source(..), id
    , inline, concatInline, inlineWithIcon14
    , icon, badge10, badge14
    , styles
    )

{-| Data source Molecule.

@docs Source, id
@docs inline, concatInline, inlineWithIcon14
@docs icon, badge10, badge14
@docs styles

-}

import Html exposing (Attribute, Html, span)
import Html.Attributes exposing (class)
import Octicons
import View.Atoms.Image as Image
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


{-| Renders a list of inline Html nodes from a Source.
-}
inline : Int -> Source -> List (Html msg)
inline octiconSize source =
    case source of
        DiscordSource { name } ->
            [ t ("#" ++ name) ]

        SlackSource { name, isPrivate } ->
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


badge10 : Source -> Html msg
badge10 source =
    case source of
        DiscordSource _ ->
            Icon.discord10

        SlackSource _ ->
            Icon.slack10


badge14 : Source -> Html msg
badge14 source =
    case source of
        DiscordSource _ ->
            Icon.discord14

        SlackSource _ ->
            Icon.slack14


inlineWithIcon14 : Source -> Html msg
inlineWithIcon14 source =
    span [] <|
        [ badge14 source
        , t " "
        , icon [ Icon.rounded14, regular ] source
        , t " "
        ]
            ++ inline regularSize source



-- STYLES


styles : List Style
styles =
    [ s (descOf (c inlineLockIconClass) (c "octicon")) [ ( "vertical-align", "bottom" ) ] -- Adjusting lock icons' position
    ]


inlineLockIconClass : String
inlineLockIconClass =
    "illock"
