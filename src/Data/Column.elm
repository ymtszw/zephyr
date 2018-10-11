module Data.Column exposing
    ( Column, Filter(..), FilterAtom(..), MediaFilter(..), MetadataFilter(..), welcome, encoder, decoder
    , foldFilter, mapFilter, indexedMapFilter, appendToFilter, setAtFilter
    )

{-| Types and functions for columns in Zephyr.


## Types

@docs Column, Filter, FilterAtom, MediaFilter, MetadataFilter, welcome, encoder, decoder


## Filter APIs

@docs foldFilter, mapFilter, indexedMapFilter, appendToFilter, setAtFilter

-}

import Array exposing (Array)
import Data.Item as Item exposing (Item)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


type alias Column =
    { id : String
    , items : List Item
    , filters : Array Filter
    , configOpen : Bool
    }


{-| Filter to narrow down Items flowing into a Column.

An Array (List) of Filters showld work in logical "and" manner.
If newly arriving Item meets ALL Filters, it should enter the Column.

Filter type itself combines multiple filter conditions (FilterAtoms) in "or" manner.
This is basically a linked list, though root node corresponds to an actual element, rather than emptiness.
Therefore this data structure shows similar characteristics to Lists.
Obviously prepending is fast while appending is slow (_O(N)_).
Actual logical "or" processing will be done from "left-to-right" manner (foldl).

However, adding new FilterAtom in a Filter is better be implemented in "append" manner,
since IMO, that matches better with users' expectations in those kind of GUIs.
For that, this module reluctantly exposes `appendToFilter` API.

-}
type Filter
    = Singular FilterAtom
    | Or FilterAtom Filter


type FilterAtom
    = ByMessage String
    | ByMedia MediaFilter
    | ByMetadata MetadataFilter


type MediaFilter
    = HasNone
    | HasImage
    | HasMovie


type MetadataFilter
    = IsDiscord
    | OfDiscordGuild String
    | OfDiscordChannel String
    | IsDefault


decoder : Decoder Column
decoder =
    D.map4 Column
        (D.field "id" D.string)
        (D.field "items" (D.list Item.decoder))
        (D.oneOf
            [ D.field "filters" (D.array filterDecoder)
            , D.succeed Array.empty -- Migration
            ]
        )
        (D.succeed False)


filterDecoder : Decoder Filter
filterDecoder =
    D.oneOf
        [ D.tagged "Singular" Singular filterAtomDecoder
        , D.tagged2 "Or" Or filterAtomDecoder (D.lazy (\_ -> filterDecoder))
        ]


filterAtomDecoder : Decoder FilterAtom
filterAtomDecoder =
    D.oneOf
        [ D.tagged "ByMessage" ByMessage D.string
        , D.tagged "ByMedia" ByMedia mediaTypeDecoder
        , D.tagged "ByMetadata" ByMetadata metadataFilterDecoder
        ]


mediaTypeDecoder : Decoder MediaFilter
mediaTypeDecoder =
    D.oneOf [ D.tag "HasNone" HasNone, D.tag "HasImage" HasImage, D.tag "HasMovie" HasMovie ]


metadataFilterDecoder : Decoder MetadataFilter
metadataFilterDecoder =
    D.oneOf
        [ D.tag "IsDiscord" IsDiscord
        , D.tagged "OfDiscordGuild" OfDiscordGuild D.string
        , D.tagged "OfDiscordChannel" OfDiscordChannel D.string
        , D.tag "IsDefault" IsDefault
        ]


encoder : Column -> E.Value
encoder { id, items, filters } =
    E.object
        [ ( "id", E.string id )
        , ( "items", E.list Item.encoder items )
        , ( "filters", E.array encodeFilter filters )
        ]


encodeFilter : Filter -> E.Value
encodeFilter filter =
    case filter of
        Singular filterAtom ->
            E.tagged "Singular" (encodeFilterAtom filterAtom)

        Or filterAtom otherFilter ->
            E.tagged2 "Or" (encodeFilterAtom filterAtom) (encodeFilter otherFilter)


encodeFilterAtom : FilterAtom -> E.Value
encodeFilterAtom filterAtom =
    case filterAtom of
        ByMessage query ->
            E.tagged "ByMessage" (E.string query)

        ByMedia mediaType ->
            E.tagged "ByMedia" (encodeMediaType mediaType)

        ByMetadata metadataFilter ->
            E.tagged "ByMetadata" (encodeMetadataFilter metadataFilter)


encodeMediaType : MediaFilter -> E.Value
encodeMediaType mediaType =
    case mediaType of
        HasNone ->
            E.tag "HasNone"

        HasImage ->
            E.tag "HasImage"

        HasMovie ->
            E.tag "HasMovie"


encodeMetadataFilter : MetadataFilter -> E.Value
encodeMetadataFilter metadataFilter =
    case metadataFilter of
        IsDiscord ->
            E.tag "IsDiscord"

        OfDiscordGuild guildId ->
            E.tagged "OfDiscordGuild" (E.string guildId)

        OfDiscordChannel channelId ->
            E.tagged "OfDiscordChannel" (E.string channelId)

        IsDefault ->
            E.tag "IsDefault"


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ Item.welcome
        , Item.textOnly "Text only message is also possible!"
        , Item.textOnly "URLs in message are automatically linkified by default.\nSource code of this project can be found at https://github.com/ymtszw/zephyr !\nAnd zephyr is powered by outstanding Elm language! https://elm-lang.org\nAlso, zephyr utilizes ServiceWorker and other technologies for Progressive Web App! https://developers.google.com/web/progressive-web-apps/\nIt works offline! Try it out!."
        , Item.textOnly "Design is obviously inspired by Tweetdeck. Scrollbar style is only applied in webkit-family browsers."
        , Item.textOnly "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        ]
            |> List.repeat 2
            |> List.concat
    , filters = Array.empty
    , configOpen = False
    }



-- FILTER APIs


foldFilter : (FilterAtom -> a -> a) -> a -> Filter -> a
foldFilter reducer acc filter =
    case filter of
        Singular filterAtom ->
            reducer filterAtom acc

        Or filterAtom otherFilter ->
            foldFilter reducer (reducer filterAtom acc) otherFilter


mapFilter : (FilterAtom -> a) -> Filter -> List a
mapFilter transform filter =
    filter
        |> foldFilter (\fa acc -> transform fa :: acc) []
        |> List.reverse


{-| Purpose-oriented fmap of Filter.

In View, we want to show/edit Filter.
In that, edit should be done via editing FilterAtom one by one.
Therefore, we must be able to edit a FilterAtom wihtin a Filter,
THEN get partially-edited Filter for later use (namely, sending via Msg).

This variant of fmap achieves such a goal.
Transformer function will receive not just a FilterAtom,
but also a FUNCTION that yields a new, partially-updated Filter when a new FilterAtom is given.
This "updater" function does not alter any other FilterAtoms in the Filter,
just updates one at the position where that particular function has effect.

Index can be used to e.g. assign unique ID to derived DOM element.

-}
indexedMapFilter : (Int -> FilterAtom -> a) -> Filter -> List a
indexedMapFilter transform filter =
    List.reverse (indexedMapFiltterImpl transform 0 [] filter)


indexedMapFiltterImpl : (Int -> FilterAtom -> a) -> Int -> List a -> Filter -> List a
indexedMapFiltterImpl transform index acc filter =
    case filter of
        Singular filterAtom ->
            transform index filterAtom :: acc

        Or filterAtom otherFilter ->
            indexedMapFiltterImpl transform (index + 1) (transform index filterAtom :: acc) otherFilter


{-| Add a FilterAtom at the end of "Or" sequence of a Filter. Takes _O(N)_.
-}
appendToFilter : FilterAtom -> Filter -> Filter
appendToFilter newFilterAtom filter =
    appendToFilterImpl newFilterAtom [] filter


appendToFilterImpl : FilterAtom -> List FilterAtom -> Filter -> Filter
appendToFilterImpl newFilterAtom reversedFilterAtoms filter =
    case filter of
        Singular filterAtom ->
            prependAccumulated reversedFilterAtoms (Or filterAtom (Singular newFilterAtom))

        Or filterAtom otherFilter ->
            appendToFilterImpl newFilterAtom (filterAtom :: reversedFilterAtoms) otherFilter


prependAccumulated : List FilterAtom -> Filter -> Filter
prependAccumulated reversedFilterAtoms filter =
    case reversedFilterAtoms of
        [] ->
            filter

        fa :: fas ->
            prependAccumulated fas (Or fa filter)


setAtFilter : Int -> FilterAtom -> Filter -> Filter
setAtFilter targetIndex newFilterAtom filter =
    setAtFilterImpl targetIndex newFilterAtom 0 [] filter


setAtFilterImpl : Int -> FilterAtom -> Int -> List FilterAtom -> Filter -> Filter
setAtFilterImpl targetIndex newFilterAtom index reversedFilterAtoms filter =
    if targetIndex == index then
        case filter of
            Singular _ ->
                prependAccumulated reversedFilterAtoms (Singular newFilterAtom)

            Or _ rest ->
                prependAccumulated reversedFilterAtoms (Or newFilterAtom rest)

    else
        case filter of
            Singular _ ->
                -- Reached end without matching index (out-of-bound); just backtrack
                prependAccumulated reversedFilterAtoms filter

            Or filterAtom rest ->
                setAtFilterImpl targetIndex newFilterAtom (index + 1) (filterAtom :: reversedFilterAtoms) rest
