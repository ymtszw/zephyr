module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder
    , add, get, indexedMap, removeAt, updateById, applyOrder
    , discordChannelIds, consumeBroker
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict,
whereas their order is stored in Array of IDs.


## Types

@docs ColumnStore, init, encode, decoder


## APIs

@docs add, get, indexedMap, removeAt, updateById, applyOrder


## Producer/Consumer APIs

@docs discordChannelIds, consumeBroker

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker)
import Data.Column as Column exposing (Column)
import Data.Filter exposing (FilterAtom(..))
import Data.Item exposing (Item)
import Dict exposing (Dict)
import Extra exposing (map, pure)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set


type alias ColumnStore =
    { dict : Dict String Column
    , order : Array String
    }


decoder : Int -> Decoder ColumnStore
decoder clientHeight =
    D.map2 ColumnStore
        (D.field "dict" (D.dict (Column.decoder clientHeight)))
        (D.field "order" (D.array D.string))


encode : ColumnStore -> E.Value
encode { dict, order } =
    E.object
        [ ( "dict", E.dict identity Column.encode dict )
        , ( "order", E.array E.string order )
        ]


init : ColumnStore
init =
    ColumnStore Dict.empty Array.empty



-- SINGULAR APIs


add : Column -> ColumnStore -> ColumnStore
add column columnStore =
    { columnStore
        | dict = Dict.insert column.id column columnStore.dict
        , order = Array.squeeze 0 column.id columnStore.order
    }


get : Int -> ColumnStore -> Maybe Column
get index columnStore =
    columnStore.order
        |> Array.get index
        |> Maybe.andThen (\id -> Dict.get id columnStore.dict)


removeAt : Int -> ColumnStore -> ColumnStore
removeAt index columnStore =
    case Array.get index columnStore.order of
        Just id ->
            { columnStore
                | dict = Dict.remove id columnStore.dict
                , order = Array.removeAt index columnStore.order
            }

        Nothing ->
            columnStore


updateById : String -> Column.Msg -> ColumnStore -> ( ColumnStore, Cmd Column.Msg, Bool )
updateById cId cMsg columnStore =
    case Dict.get cId columnStore.dict of
        Just c ->
            Column.update cMsg c
                |> map (\newC -> { columnStore | dict = Dict.insert cId newC columnStore.dict }) identity

        Nothing ->
            pure columnStore



-- BULK APIs


indexedMap : (Int -> Column -> a) -> ColumnStore -> List a
indexedMap mapper { dict, order } =
    indexedMapImpl mapper dict (Array.toList order) 0 []


indexedMapImpl : (Int -> Column -> a) -> Dict String Column -> List String -> Int -> List a -> List a
indexedMapImpl mapper dict idList index acc =
    case idList of
        [] ->
            List.reverse acc

        id :: ids ->
            case Dict.get id dict of
                Just column ->
                    indexedMapImpl mapper dict ids (index + 1) (mapper index column :: acc)

                Nothing ->
                    -- Should not happen as long as contents of ColumnStore are manipulated by functions in this module
                    indexedMapImpl mapper dict ids index acc


applyOrder : Array String -> ColumnStore -> ColumnStore
applyOrder order columnStore =
    { columnStore | order = order }


consumeBroker : Broker Item -> ColumnStore -> ( ColumnStore, Bool )
consumeBroker broker columnStore =
    let
        ( newDict, shouldPersist ) =
            Dict.foldl reducer ( Dict.empty, False ) columnStore.dict

        scanCountPerColumn =
            maxScanCount // Dict.size columnStore.dict

        reducer cId column ( accDict, accSP ) =
            column
                |> Column.consumeBroker scanCountPerColumn broker
                |> Tuple.mapBoth
                    (\newColumn -> Dict.insert cId newColumn accDict)
                    ((||) accSP)
    in
    ( { columnStore | dict = newDict }, shouldPersist )


maxScanCount : Int
maxScanCount =
    500



-- Producer APIs


{-| Enumerate Discord channel IDs which are currently subscribed.
-}
discordChannelIds : ColumnStore -> List String
discordChannelIds columnStore =
    let
        channelIdInFilterAtom filterAtom accSet =
            case filterAtom of
                OfDiscordChannel cId ->
                    Set.insert cId accSet

                _ ->
                    accSet
    in
    columnStore.dict
        |> Dict.foldl
            (\_ c s -> Array.foldl (\f ss -> Data.Filter.fold channelIdInFilterAtom ss f) s c.filters)
            Set.empty
        |> Set.toList
