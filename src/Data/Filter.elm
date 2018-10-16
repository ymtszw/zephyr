module Data.Filter exposing
    ( Filter(..), FilterAtom(..), MediaFilter(..), encode, decoder
    , append, setAt, removeAt, updateAt, fold, map, indexedMap
    )

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
For that, this module reluctantly exposes `append` API.


## Types

@docs Filter, FilterAtom, MediaFilter, encode, decoder


## APIs

@docs append, setAt, removeAt, updateAt, fold, map, indexedMap

-}

import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


type Filter
    = Singular FilterAtom
    | Or FilterAtom Filter


type FilterAtom
    = ByMessage String
    | ByMedia MediaFilter
    | IsDiscord
    | OfDiscordGuild String
    | OfDiscordChannel String
    | IsSystem
    | RemoveMe -- This is only for deletion from UI, not actually used as filter


type MediaFilter
    = HasNone
    | HasImage
    | HasMovie


encode : Filter -> E.Value
encode filter =
    case filter of
        Singular filterAtom ->
            E.tagged "Singular" (encodeFilterAtom filterAtom)

        Or filterAtom otherFilter ->
            E.tagged2 "Or" (encodeFilterAtom filterAtom) (encode otherFilter)


encodeFilterAtom : FilterAtom -> E.Value
encodeFilterAtom filterAtom =
    case filterAtom of
        ByMessage query ->
            E.tagged "ByMessage" (E.string query)

        ByMedia mediaType ->
            E.tagged "ByMedia" (encodeMediaType mediaType)

        IsDiscord ->
            E.tag "IsDiscord"

        OfDiscordGuild guildId ->
            E.tagged "OfDiscordGuild" (E.string guildId)

        OfDiscordChannel channelId ->
            E.tagged "OfDiscordChannel" (E.string channelId)

        IsSystem ->
            E.tag "IsDefault"

        RemoveMe ->
            -- Should not be stored in Filter
            encodeFilterAtom filterAtom


encodeMediaType : MediaFilter -> E.Value
encodeMediaType mediaType =
    case mediaType of
        HasNone ->
            E.tag "HasNone"

        HasImage ->
            E.tag "HasImage"

        HasMovie ->
            E.tag "HasMovie"


decoder : Decoder Filter
decoder =
    D.oneOf
        [ D.tagged "Singular" Singular filterAtomDecoder
        , D.tagged2 "Or" Or filterAtomDecoder (D.lazy (\_ -> decoder))
        ]


filterAtomDecoder : Decoder FilterAtom
filterAtomDecoder =
    D.oneOf
        [ D.tagged "ByMessage" ByMessage D.string
        , D.tagged "ByMedia" ByMedia mediaTypeDecoder
        , D.tag "IsDiscord" IsDiscord
        , D.tagged "OfDiscordGuild" OfDiscordGuild D.string
        , D.tagged "OfDiscordChannel" OfDiscordChannel D.string
        , D.tag "IsSystem" IsSystem

        -- Old version
        , D.tagged "ByMetadata" identity oldMetadataFilterDecoder
        ]


mediaTypeDecoder : Decoder MediaFilter
mediaTypeDecoder =
    D.oneOf [ D.tag "HasNone" HasNone, D.tag "HasImage" HasImage, D.tag "HasMovie" HasMovie ]


oldMetadataFilterDecoder : Decoder FilterAtom
oldMetadataFilterDecoder =
    D.oneOf
        [ D.tag "IsDiscord" IsDiscord
        , D.tagged "OfDiscordGuild" OfDiscordGuild D.string
        , D.tagged "OfDiscordChannel" OfDiscordChannel D.string
        , D.tag "IsDefault" IsSystem
        ]


fold : (FilterAtom -> a -> a) -> a -> Filter -> a
fold reducer acc filter =
    case filter of
        Singular filterAtom ->
            reducer filterAtom acc

        Or filterAtom otherFilter ->
            fold reducer (reducer filterAtom acc) otherFilter


map : (FilterAtom -> a) -> Filter -> List a
map transform filter =
    filter
        |> fold (\fa acc -> transform fa :: acc) []
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
indexedMap : (Int -> FilterAtom -> a) -> Filter -> List a
indexedMap transform filter =
    List.reverse (indexedMapImpl transform 0 [] filter)


indexedMapImpl : (Int -> FilterAtom -> a) -> Int -> List a -> Filter -> List a
indexedMapImpl transform index acc filter =
    case filter of
        Singular filterAtom ->
            transform index filterAtom :: acc

        Or filterAtom otherFilter ->
            indexedMapImpl transform (index + 1) (transform index filterAtom :: acc) otherFilter


{-| Add a FilterAtom at the end of "Or" sequence of a Filter. Takes _O(N)_.
-}
append : FilterAtom -> Filter -> Filter
append newFilterAtom filter =
    appendImpl newFilterAtom [] filter


appendImpl : FilterAtom -> List FilterAtom -> Filter -> Filter
appendImpl newFilterAtom reversedFilterAtoms filter =
    case filter of
        Singular filterAtom ->
            prependAccumulated reversedFilterAtoms (Or filterAtom (Singular newFilterAtom))

        Or filterAtom otherFilter ->
            appendImpl newFilterAtom (filterAtom :: reversedFilterAtoms) otherFilter


prependAccumulated : List FilterAtom -> Filter -> Filter
prependAccumulated reversedFilterAtoms filter =
    case reversedFilterAtoms of
        [] ->
            filter

        fa :: fas ->
            prependAccumulated fas (Or fa filter)


setAt : Int -> FilterAtom -> Filter -> Filter
setAt targetIndex newFilterAtom filter =
    updateAt targetIndex (always (Just newFilterAtom)) filter |> Maybe.withDefault filter


removeAt : Int -> Filter -> Maybe Filter
removeAt targetIndex filter =
    updateAt targetIndex (always Nothing) filter


updateAt : Int -> (FilterAtom -> Maybe FilterAtom) -> Filter -> Maybe Filter
updateAt targetIndex update filter =
    updateAtImpl targetIndex update 0 [] filter


updateAtImpl : Int -> (FilterAtom -> Maybe FilterAtom) -> Int -> List FilterAtom -> Filter -> Maybe Filter
updateAtImpl targetIndex update index reversedFilterAtoms filter =
    if targetIndex == index then
        case ( filter, reversedFilterAtoms ) of
            ( Singular targetFilterAtom, [] ) ->
                case update targetFilterAtom of
                    Just newFilterAtom ->
                        Just (Singular newFilterAtom)

                    Nothing ->
                        Nothing

            ( Singular targetFilterAtom, prevFilterAtom :: fas ) ->
                case update targetFilterAtom of
                    Just newFilterAtom ->
                        Just (prependAccumulated reversedFilterAtoms (Singular newFilterAtom))

                    Nothing ->
                        Just (prependAccumulated fas (Singular prevFilterAtom))

            ( Or targetFilterAtom rest, [] ) ->
                case update targetFilterAtom of
                    Just newFilterAtom ->
                        Just (Or newFilterAtom rest)

                    Nothing ->
                        Just rest

            ( Or targetFilterAtom rest, prevFilterAtom :: fas ) ->
                case update targetFilterAtom of
                    Just newFilterAtom ->
                        Just (prependAccumulated reversedFilterAtoms (Or newFilterAtom rest))

                    Nothing ->
                        Just (prependAccumulated fas (Or prevFilterAtom rest))

    else
        case filter of
            Singular _ ->
                -- Reached end without matching index (out-of-bound); just backtrack
                Just (prependAccumulated reversedFilterAtoms filter)

            Or filterAtom rest ->
                updateAtImpl targetIndex update (index + 1) (filterAtom :: reversedFilterAtoms) rest