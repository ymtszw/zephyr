module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder, storeId, size
    , add, get, map, mapForView, removeAt, touchAt
    , updateById, applyOrder, consumeBroker, catchUpBroker, updateFAM
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict,
whereas their order is stored in Array of IDs.

@docs ColumnStore, init, encode, decoder, storeId, size
@docs add, get, map, mapForView, removeAt, touchAt
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
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E


type alias ColumnStore =
    { dict : Dict String Column
    , order : Array String
    , fam : FilterAtomMaterial
    , indexToScan : Int
    }


decoder : Int -> Decoder ColumnStore
decoder clientHeight =
    D.do (D.field "dict" (D.dict (Column.decoder clientHeight))) <|
        \dict ->
            D.do (D.field "order" (D.array D.string)) <|
                \order ->
                    D.do (D.maybeField "fam" FAM.decoder |> D.map (Maybe.withDefault FAM.init)) <|
                        \fam ->
                            D.succeed (ColumnStore dict order fam 0)


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
    ColumnStore Dict.empty Array.empty FAM.init 0


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
    in
    { columnStore | dict = newDict, order = newOrder }


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


touchAt : Int -> ColumnStore -> ColumnStore
touchAt index columnStore =
    case get index columnStore of
        Just c ->
            { columnStore | dict = Dict.insert c.id { c | recentlyTouched = True } columnStore.dict }

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
    case get columnStore.indexToScan columnStore of
        Just column ->
            let
                scanCountPerColumn =
                    maxScanCount // Dict.size columnStore.dict

                ( newColumn, pp ) =
                    column
                        |> Column.adjustScroll clientHeight
                        |> Column.update (Column.ScanBroker { broker = broker, maxCount = scanCountPerColumn })

                nextIndex =
                    if columnStore.indexToScan < Array.length columnStore.order - 1 then
                        columnStore.indexToScan + 1

                    else
                        0
            in
            ( { columnStore | indexToScan = nextIndex, dict = Dict.insert column.id newColumn columnStore.dict }, pp )

        Nothing ->
            pure { columnStore | indexToScan = 0 }


maxScanCount : Int
maxScanCount =
    500


catchUpBroker : Broker Item -> String -> ColumnStore -> ( ColumnStore, Column.PostProcess )
catchUpBroker broker cId columnStore =
    updateById cId (Column.ScanBroker { broker = broker, maxCount = maxScanCount }) columnStore


updateFAM : List UpdateInstruction -> ColumnStore -> ( ColumnStore, Bool )
updateFAM instructions columnStore =
    FAM.update instructions columnStore.fam
        |> Tuple.mapFirst (\newFAM -> { columnStore | fam = newFAM })
