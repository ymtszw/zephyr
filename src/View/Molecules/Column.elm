module View.Molecules.Column exposing
    ( ColumnProps
    , inlineTitle, blockTitle, icon20, icon30, icon40
    )

{-| Molecules for Column-related UI parts.

@docs ColumnProps
@docs inlineTitle, blockTitle, icon20, icon30, icon40
@docs styles

-}

import Html exposing (Attribute, Html, div)
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (clip, ellipsis, nowrap)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.Source as Source exposing (Source(..))
import View.Style exposing (..)


type alias ColumnProps c =
    { c
        | pinned : Bool
        , sources : List Source -- Empty list indicates "any" sources
        , filters : List String
    }


{-| Renders a list of inline Html nodes from sources and filters.

Meant to be wrapped by a styled node.

-}
inlineTitle : Int -> ColumnProps c -> List (Html msg)
inlineTitle octiconSize cp =
    case ( cp.sources, cp.filters ) of
        ( [], [] ) ->
            [ t "New Column" ]

        ( sources, [] ) ->
            Source.concatInline octiconSize sources

        ( [], filters ) ->
            [ t (String.join ", " filters) ]

        ( sources, filters ) ->
            Source.concatInline octiconSize sources
                ++ [ t ", ", t (String.join ", " filters) ]


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
            div [ bold, prominent, ellipsis ]
    in
    div (baseAttrs ++ userAttrs) <|
        case ( cp.sources, cp.filters ) of
            ( [], [] ) ->
                [ mainText [ t "New Column" ] ]

            ( sources, [] ) ->
                [ mainText (Source.concatInline prominentSize sources) ]

            ( [], filters ) ->
                [ mainText [ t (String.join ", " filters) ] ]

            ( sources, filters ) ->
                [ mainText (Source.concatInline prominentSize sources)
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
                ( Just (Source.badge10 [] source), Source.icon [ Icon.rounded20 ] source )


{-| Renders a badged icon representing a column in 30x30 size.

Meant to be used in column headers.

-}
icon30 : ColumnProps c -> Html msg
icon30 cp =
    badgedIcon [ badgeOutset ] Nothing <|
        case cp.sources of
            [] ->
                ( Nothing, Icon.abbr [ Icon.rounded30, serif, xProminent ] "Zephyr" )

            source :: _ ->
                ( Just (Source.badge14 [] source), Source.icon [ Icon.rounded30, xProminent ] source )


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
                ( Nothing, Icon.abbr [ Icon.rounded40, serif, xProminent ] "Zephyr" )

            source :: _ ->
                ( Just (Source.badge14 [] source), Source.icon [ Icon.rounded40, xProminent ] source )


badgedIcon : List (Attribute msg) -> Maybe (Html msg) -> ( Maybe (Html msg), Html msg ) -> Html msg
badgedIcon attrs topRight ( bottomRight, content ) =
    withBadge attrs
        { topRight = topRight
        , bottomRight = bottomRight
        , content = content
        }
