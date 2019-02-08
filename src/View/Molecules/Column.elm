module View.Molecules.Column exposing
    ( ColumnProps, Source(..), sourceId
    , inlineTitle, blockTitle, icon20, icon30, icon40, sourceSummary14
    , styles
    )

{-| Molecules for Column-related UI parts.

@docs ColumnProps, Source, sourceId
@docs inlineTitle, blockTitle, icon20, icon30, icon40, sourceSummary14
@docs styles

-}

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Octicons
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip, ellipsis, nowrap)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias ColumnProps c =
    { c
        | pinned : Bool
        , sources : List Source -- Empty list indicates "any" sources
        , filters : List String
    }


{-| Representation of data sources for columns.
-}
type Source
    = DiscordSource
        { id : String
        , channelName : String
        , guildName : String -- Currently DM/GroupDM are not supported
        , guildIcon : Maybe String
        }
    | SlackSource
        { id : String
        , convName : String
        , teamName : String
        , teamIcon : Maybe String
        , isPrivate : Bool
        }


sourceId : Source -> String
sourceId source =
    case source of
        DiscordSource { id } ->
            "DiscordSource_" ++ id

        SlackSource { id } ->
            "SlackSource_" ++ id


{-| Renders a list of inline Html nodes from sources and filters.

Meant to be wrapped by a styled node.

-}
inlineTitle : Int -> ColumnProps c -> List (Html msg)
inlineTitle octiconSize cp =
    case ( cp.sources, cp.filters ) of
        ( [], [] ) ->
            [ t "New Column" ]

        ( sources, [] ) ->
            sourcesInline octiconSize sources

        ( [], filters ) ->
            [ t (String.join ", " filters) ]

        ( sources, filters ) ->
            sourcesInline octiconSize sources
                ++ [ t ", ", t (String.join ", " filters) ]


sourcesInline : Int -> List Source -> List (Html msg)
sourcesInline octiconSize sources =
    sources |> List.map (inlineSource octiconSize) |> List.intersperse [ t ", " ] |> List.concat


{-| Renders a list of inline Html nodes from a Source.
-}
inlineSource : Int -> Source -> List (Html msg)
inlineSource octiconSize source =
    case source of
        DiscordSource { channelName } ->
            [ t ("#" ++ channelName) ]

        SlackSource { convName, isPrivate } ->
            [ if isPrivate then
                span [ class inlineLockIconClass, Image.fillText ]
                    [ Image.octicon { size = octiconSize, shape = Octicons.lock } ]

              else
                t "#"
            , t convName
            ]


{-| Renders a column block consists of text information of sources and filters.

Text contents in this block are never wrapped.
If constrained (using e.g. `max-width`), it clips overflown text with ellipsis.

-}
blockTitle : List (Attribute msg) -> ColumnProps c -> Html msg
blockTitle userAttrs cp =
    let
        baseAttrs =
            [ flexShrink, flexColumn, spacingColumn2, nowrap, clip ]

        mainText =
            div [ bold, headline, ellipsis ]

        octiconSize =
            13
    in
    div (baseAttrs ++ userAttrs) <|
        case ( cp.sources, cp.filters ) of
            ( [], [] ) ->
                [ mainText [ t "New Column" ] ]

            ( sources, [] ) ->
                [ mainText (sourcesInline octiconSize sources) ]

            ( [], filters ) ->
                [ mainText [ t (String.join ", " filters) ] ]

            ( sources, filters ) ->
                [ mainText (sourcesInline octiconSize sources)
                , div [ colorNote, minuscule, ellipsis ] [ t (String.join ", " filters) ]
                ]


{-| Renders a badged icon representing a column in 20x20 size.

Meant to be used in shadow columns table.

-}
icon20 : ColumnProps c -> Html msg
icon20 cp =
    badgedIcon [{- No badgeOutset -}] Nothing <|
        case cp.sources of
            [] ->
                ( Nothing, Icon.abbr [ Icon.rounded20, serif ] "Zephyr" )

            source :: _ ->
                ( Just (sourceBadge10 source), sourceIcon [ Icon.rounded20 ] source )


sourceBadge10 : Source -> Html msg
sourceBadge10 source =
    case source of
        DiscordSource _ ->
            Icon.discord10

        SlackSource _ ->
            Icon.slack10


sourceIcon : List (Attribute msg) -> Source -> Html msg
sourceIcon attrs source =
    case source of
        DiscordSource opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.guildName opts.guildIcon

        SlackSource opts ->
            Icon.imgOrAbbr (serif :: attrs) opts.teamName opts.teamIcon


{-| Renders a badged icon representing a column in 30x30 size.

Meant to be used in column headers.

-}
icon30 : ColumnProps c -> Html msg
icon30 cp =
    badgedIcon [ badgeOutset ] Nothing <|
        case cp.sources of
            [] ->
                ( Nothing, Icon.abbr [ Icon.rounded30, serif, sizeTitle ] "Zephyr" )

            source :: _ ->
                ( Just (sourceBadge14 source), sourceIcon [ Icon.rounded30, sizeTitle ] source )


sourceBadge14 : Source -> Html msg
sourceBadge14 source =
    case source of
        DiscordSource _ ->
            Icon.discord14

        SlackSource _ ->
            Icon.slack14


{-| Renders a pinned/badged icon representing a column in 40x40 size.

Meant to be used in the sidebar.

-}
icon40 : ColumnProps c -> Html msg
icon40 cp =
    let
        topRight =
            if cp.pinned then
                Just Icon.pinBadge14

            else
                Nothing
    in
    badgedIcon [ badgeOutset ] topRight <|
        case cp.sources of
            [] ->
                ( Nothing, Icon.abbr [ Icon.rounded40, serif, sizeTitle ] "Zephyr" )

            source :: _ ->
                ( Just (sourceBadge14 source), sourceIcon [ Icon.rounded40, sizeTitle ] source )


badgedIcon : List (Attribute msg) -> Maybe (Html msg) -> ( Maybe (Html msg), Html msg ) -> Html msg
badgedIcon attrs topRight ( bottomRight, content ) =
    withBadge attrs
        { topRight = topRight
        , bottomRight = bottomRight
        , content = content
        }


sourceSummary14 : Source -> Html msg
sourceSummary14 source =
    let
        octiconSize =
            -- regular
            12
    in
    div [ flexRow, flexCenter, spacingRow2, clip ]
        [ div [ flexItem, flexBasisAuto ] [ sourceBadge14 source ]
        , sourceIcon [ flexItem, flexBasisAuto, Icon.rounded14, regular ] source
        , div [ flexGrow, flexBasisAuto, nowrap, ellipsis ] (inlineSource octiconSize source)
        ]


styles : List Style
styles =
    [ s (descOf (c inlineLockIconClass) (c "octicon")) [ ( "vertical-align", "bottom" ) ] -- Adjusting lock icons' position
    ]


inlineLockIconClass : String
inlineLockIconClass =
    "illock"
