module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder
    , add, get, indexedMap, removeAt, updateById, applyOrder
    , discordChannelIds
    , pushToFirstColumn
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict,
whereas their order is stored in Array of IDs.


## Types

@docs ColumnStore, init, encode, decoder


## APIs

@docs add, get, indexedMap, removeAt, updateById, applyOrder


## Producer APIs

@docs discordChannelIds


## Debug

@docs pushToFirstColumn

-}

import Array exposing (Array)
import Data.Array as Array
import Data.Column as Column exposing (Column)
import Data.Filter exposing (FilterAtom(..))
import Data.Item exposing (Item)
import Data.UniqueId
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set


type alias ColumnStore =
    { dict : Dict String Column
    , order : Array String
    }


decoder : Decoder ColumnStore
decoder =
    D.map2 ColumnStore
        (D.field "dict" (D.dict Column.decoder))
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
        , order = Array.push column.id columnStore.order
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


updateById : String -> (Column -> Column) -> ColumnStore -> ColumnStore
updateById id transform columnStore =
    { columnStore | dict = Dict.update id (Maybe.map transform) columnStore.dict }



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



-- DEBUG


{-| TODO Use Broker and Consumer (Column) read/filter mechanism.

This is mostly for debugging purposes; take list of Items, push them all to the leftmost Column.
If there is no Column in ColumnStore, generate one.

-}
pushToFirstColumn : Data.UniqueId.Generator -> List Item -> ColumnStore -> ( ColumnStore, Data.UniqueId.Generator )
pushToFirstColumn idGen items columnStore =
    case get 0 columnStore of
        Just column ->
            ( { columnStore | dict = Dict.insert column.id { column | items = List.reverse items ++ column.items } columnStore.dict }, idGen )

        Nothing ->
            let
                ( id, newIdGen ) =
                    Data.UniqueId.gen "column" idGen
            in
            ( add (Column id (List.reverse items) Array.empty Nothing False "") columnStore, newIdGen )
