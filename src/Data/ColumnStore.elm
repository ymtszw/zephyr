module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder, storeId
    , add, get, map, mapForView, removeAt
    , updateById, applyOrder, consumeBroker, updateFAM
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict,
whereas their order is stored in Array of IDs.

@docs ColumnStore, init, encode, decoder, storeId
@docs add, get, map, mapForView, removeAt
@docs updateById, applyOrder, consumeBroker, updateFAM

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker)
import Data.Column as Column exposing (Column)
import Data.Filter exposing (FilterAtom(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial, UpdateInstruction)
import Data.Item exposing (Item)
import Data.Storable exposing (Storable)
import Dict exposing (Dict)
import Extra exposing (pure)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Set


type alias ColumnStore =
    { dict : Dict String Column
    , order : Array String
    , fam : FilterAtomMaterial
    }


decoder : Int -> Decoder ColumnStore
decoder clientHeight =
    D.map3 ColumnStore
        (D.field "dict" (D.dict (Column.decoder clientHeight)))
        (D.field "order" (D.array D.string))
        (D.maybeField "fam" FAM.decoder |> D.map (Maybe.withDefault FAM.init))


encode : ColumnStore -> Storable
encode columnStore =
    Data.Storable.encode storeId
        [ ( "dict", E.dict identity Column.encode columnStore.dict )
        , ( "order", E.array E.string columnStore.order )
        , ( "fam", FAM.encode columnStore.fam )
        ]


storeId : String
storeId =
    "columnStore"


init : ColumnStore
init =
    ColumnStore Dict.empty Array.empty FAM.init



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



-- BULK APIs


map : (Column -> Column) -> ColumnStore -> ColumnStore
map mapper columnStore =
    { columnStore | dict = Dict.map (\_ c -> mapper c) columnStore.dict }


mapForView : (FilterAtomMaterial -> Int -> Column -> a) -> ColumnStore -> List a
mapForView mapper { dict, order, fam } =
    mapForViewImpl (mapper fam) dict (Array.toList order) 0 []


mapForViewImpl : (Int -> Column -> a) -> Dict String Column -> List String -> Int -> List a -> List a
mapForViewImpl mapper dict idList index acc =
    case idList of
        [] ->
            List.reverse acc

        id :: ids ->
            case Dict.get id dict of
                Just column ->
                    mapForViewImpl mapper dict ids (index + 1) (mapper index column :: acc)

                Nothing ->
                    -- Should not happen as long as contents of ColumnStore are manipulated by functions in this module
                    mapForViewImpl mapper dict ids index acc



-- Component APIs


updateById : String -> Column.Msg -> ColumnStore -> ( ColumnStore, Cmd Column.Msg, Bool )
updateById cId cMsg columnStore =
    case Dict.get cId columnStore.dict of
        Just c ->
            Column.update cMsg c
                |> Extra.map (\newC -> { columnStore | dict = Dict.insert cId newC columnStore.dict }) identity

        Nothing ->
            pure columnStore


applyOrder : Array String -> ColumnStore -> ColumnStore
applyOrder order columnStore =
    { columnStore | order = order }


consumeBroker : Int -> Broker Item -> ColumnStore -> ( ColumnStore, Bool )
consumeBroker clientHeight broker columnStore =
    let
        ( newDict, shouldPersist ) =
            Dict.foldl reducer ( Dict.empty, False ) columnStore.dict

        scanCountPerColumn =
            maxScanCount // Dict.size columnStore.dict

        reducer cId column ( accDict, accPersist ) =
            column
                |> Column.adjustScroll clientHeight
                |> Column.consumeBroker scanCountPerColumn broker
                |> Tuple.mapBoth
                    (\newColumn -> Dict.insert cId newColumn accDict)
                    ((||) accPersist)
    in
    ( { columnStore | dict = newDict }, shouldPersist )


maxScanCount : Int
maxScanCount =
    500


updateFAM : List UpdateInstruction -> ColumnStore -> ( ColumnStore, Bool )
updateFAM instructions columnStore =
    FAM.update instructions columnStore.fam
        |> Tuple.mapFirst (\newFAM -> { columnStore | fam = newFAM })
