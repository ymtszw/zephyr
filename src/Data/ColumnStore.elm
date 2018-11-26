module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder, storeId, size
    , add, get, show, map, mapForView, listShadow, removeAt, touchAt, dismissAt
    , updateById, applyOrder, consumeBroker, catchUpBroker, updateFAM
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict,
whereas their order is stored in Array of IDs.

If a Column exists in the `dict` but its ID is not found in the `order` Array,
it is considered as a "shadow" Column. Shadow Columns are updated normally,
and automatically become visible in "Zephyr mode", if new messages arrived.

In "Zephyr mode", Columns are automatically evicted (dismissed)
when there are too many Columns displayed.
This can be toggled at users' preferences. See Data.Model.

@docs ColumnStore, init, encode, decoder, storeId, size
@docs add, get, show, map, mapForView, listShadow, removeAt, touchAt, dismissAt
@docs updateById, applyOrder, consumeBroker, catchUpBroker, updateFAM

-}

import Array exposing (Array)
import ArrayExtra as Array
import Broker exposing (Broker)
import Data.Column as Column exposing (Column)
import Data.Filter exposing (FilterAtom(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial, UpdateInstruction)
import Data.Item exposing (Item)
import Data.Storable exposing (Storable)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E


type alias ColumnStore =
    { dict : Dict String Column
    , order : Array String
    , fam : FilterAtomMaterial
    , scanQueue : Deque String
    }


decoder : Int -> Decoder ColumnStore
decoder clientHeight =
    D.do (D.field "dict" (D.dict (Column.decoder clientHeight))) <|
        \dict ->
            D.do (D.field "order" (D.array D.string)) <|
                \order ->
                    D.do (D.maybeField "fam" FAM.decoder |> D.map (Maybe.withDefault FAM.init)) <|
                        \fam ->
                            let
                                scanQueue =
                                    Deque.fromList (Dict.keys dict)
                            in
                            D.succeed (ColumnStore dict order fam scanQueue)


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
    ColumnStore Dict.empty Array.empty FAM.init Deque.empty


size : ColumnStore -> Int
size cs =
    Dict.size cs.dict



-- SINGULAR APIs


add : Column -> ColumnStore -> ColumnStore
add column columnStore =
    let
        newDict =
            Dict.insert column.id column columnStore.dict

        newOrder =
            columnStore.order |> Array.squeeze 0 column.id |> autoArrange newDict

        newScanQueue =
            -- Previous relative scan ordering is kept
            Deque.pushFront column.id columnStore.scanQueue
    in
    { columnStore | dict = newDict, order = newOrder, scanQueue = newScanQueue }


get : Int -> ColumnStore -> Maybe Column
get index columnStore =
    columnStore.order
        |> Array.get index
        |> Maybe.andThen (\id -> Dict.get id columnStore.dict)


show : String -> ColumnStore -> ColumnStore
show cId columnStore =
    let
        newDict =
            Dict.update cId (Maybe.map (\c -> { c | recentlyTouched = True })) columnStore.dict

        newOrder =
            columnStore.order |> Array.squeeze 0 cId |> autoArrange newDict
    in
    { columnStore | dict = newDict, order = newOrder }


removeAt : Int -> ColumnStore -> ColumnStore
removeAt index columnStore =
    case Array.get index columnStore.order of
        Just id ->
            let
                newDict =
                    Dict.remove id columnStore.dict
            in
            { columnStore
                | dict = newDict
                , order = Array.removeAt index columnStore.order
                , scanQueue = Deque.fromList (Dict.keys newDict) -- Previous scan ordering is discarded
            }

        Nothing ->
            columnStore


touchAt : Int -> ColumnStore -> ColumnStore
touchAt index columnStore =
    case get index columnStore of
        Just c ->
            { columnStore | dict = Dict.insert c.id { c | recentlyTouched = True } columnStore.dict }

        Nothing ->
            columnStore


dismissAt : Int -> ColumnStore -> ColumnStore
dismissAt index columnStore =
    { columnStore | order = Array.removeAt index columnStore.order }



-- BULK APIs


map : (Column -> Column) -> ColumnStore -> ColumnStore
map mapper columnStore =
    { columnStore | dict = Dict.map (\_ c -> mapper c) columnStore.dict }


{-| `indexedMap` intended for view usages.

It returns list of values derived from a mapper function.
The mapper function receives FAM, index number, and a Column.

It only iterates over Columns whose IDs can be found in `order` Array, i.e. visible Columns.

Other Columns are considered "shadow" Columns which is hidden from the view (at the moment).
Shadow Columns are re-introduced to the view via side-effect APIs,
where their IDs are squeezed into `order` Array.

Columns can be "dismissed" also via side-effect APIs, becoming shadow Columns,
when ther IDs are removed from the `order` Array but not permanently removed from the `dict`.

-}
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


listShadow : ColumnStore -> List Column
listShadow columnStore =
    let
        reducer cId c acc =
            if Array.all ((/=) cId) columnStore.order then
                c :: acc

            else
                acc
    in
    Dict.foldr reducer [] columnStore.dict



-- Component APIs


updateById : String -> Column.Msg -> ColumnStore -> ( ColumnStore, Column.PostProcess )
updateById cId cMsg columnStore =
    case Dict.get cId columnStore.dict of
        Just c ->
            let
                ( newC, pp ) =
                    Column.update cMsg c

                newDict =
                    Dict.insert cId newC columnStore.dict

                newOrder =
                    case cMsg of
                        Column.Pin _ ->
                            autoArrange newDict columnStore.order

                        _ ->
                            columnStore.order
            in
            ( { columnStore | dict = newDict, order = newOrder }, pp )

        Nothing ->
            pure columnStore


pure : ColumnStore -> ( ColumnStore, Column.PostProcess )
pure columnStore =
    ( columnStore, Column.PostProcess Cmd.none False Nothing )


autoArrange : Dict String Column -> Array String -> Array String
autoArrange dict order =
    let
        stableClassify cId ( accPinned, accLoose ) =
            case Dict.get cId dict of
                Just c ->
                    if c.pinned then
                        ( Array.push cId accPinned, accLoose )

                    else
                        ( accPinned, Array.push cId accLoose )

                Nothing ->
                    -- Should not happen
                    ( accPinned, accLoose )
    in
    order
        |> Array.foldl stableClassify ( Array.empty, Array.empty )
        |> (\( pinned, loose ) -> Array.append pinned loose)


applyOrder : Array String -> ColumnStore -> ColumnStore
applyOrder order columnStore =
    { columnStore | order = order }


consumeBroker : Int -> Broker Item -> ColumnStore -> ( ColumnStore, Column.PostProcess )
consumeBroker clientHeight broker columnStore =
    case Deque.popBack columnStore.scanQueue of
        ( Just cId, newScanQueue ) ->
            let
                scanCountPerColumn =
                    maxScanCount // Dict.size columnStore.dict

                scanMsg =
                    Column.ScanBroker { broker = broker, maxCount = scanCountPerColumn, clientHeight = clientHeight }
            in
            updateById cId scanMsg { columnStore | scanQueue = Deque.pushFront cId newScanQueue }

        ( Nothing, _ ) ->
            pure columnStore


maxScanCount : Int
maxScanCount =
    500


catchUpBroker : Int -> Broker Item -> String -> ColumnStore -> ( ColumnStore, Column.PostProcess )
catchUpBroker clientHeight broker cId columnStore =
    let
        scanMsg =
            Column.ScanBroker { broker = broker, maxCount = maxScanCount, clientHeight = clientHeight }
    in
    updateById cId scanMsg columnStore


updateFAM : List UpdateInstruction -> ColumnStore -> ( ColumnStore, Bool )
updateFAM instructions columnStore =
    FAM.update instructions columnStore.fam
        |> Tuple.mapFirst (\newFAM -> { columnStore | fam = newFAM })
