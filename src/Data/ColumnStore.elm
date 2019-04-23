module Data.ColumnStore exposing
    ( ColumnStore, init, encode, decoder, storeId, size, sizePinned
    , add, get, remove, touchAt, dismissAt, map, mapForView, listShadow
    , updateById, applyOrder, consumeBroker, updateFAM
    )

{-| Order-aware Column storage.

Internally, Columns themselves are stored in ID-based Dict (AssocList),
whereas their order is stored in Array of IDs.

If a Column exists in the `dict` but its ID is not found in the `order` Array,
it is considered as a "shadow" Column. Shadow Columns are updated normally,
and automatically become visible in "Zephyr mode", if new messages arrived.

In "Zephyr mode", Columns are automatically evicted (dismissed)
when there are too many Columns displayed.
This can be toggled at users' preferences. See Data.Model.

@docs ColumnStore, init, encode, decoder, storeId, size, sizePinned
@docs add, get, remove, touchAt, dismissAt, map, mapForView, listShadow
@docs updateById, applyOrder, consumeBroker, updateFAM

-}

import Array exposing (Array)
import ArrayExtra as Array
import AssocList as Dict exposing (Dict)
import Broker exposing (Broker)
import Data.Column as Column exposing (Column, Position(..))
import Data.Filter exposing (FilterAtom(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial, UpdateInstruction)
import Data.Item exposing (Item)
import Data.Storable exposing (Storable)
import Deque exposing (Deque)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Random


type alias ColumnStore =
    { dict : Dict Column.Id Column
    , order : Array Column.Id
    , fam : FilterAtomMaterial
    , scanQueue : Deque Column.Id
    , idGenSeed : Random.Seed
    }


decoder : { clientHeight : Int, posix : Int } -> Decoder ( ColumnStore, List ( Column.Id, Cmd Column.Msg ) )
decoder { clientHeight, posix } =
    D.do (D.field "order" (D.array (Id.decoder D.string))) <|
        \order ->
            D.do (D.field "dict" (dictAndInitCmdDecoder clientHeight order)) <|
                \( dict, idAndCmds ) ->
                    -- Migration; use field instead of optionField later
                    D.do (D.optionField "fam" FAM.decoder FAM.init) <|
                        \fam ->
                            let
                                scanQueue =
                                    Deque.fromList (Dict.keys dict)
                            in
                            D.succeed ( ColumnStore dict order fam scanQueue (Random.initialSeed posix), idAndCmds )


dictAndInitCmdDecoder : Int -> Array Column.Id -> Decoder ( Dict Column.Id Column, List ( Column.Id, Cmd Column.Msg ) )
dictAndInitCmdDecoder clientHeight order =
    D.do (D.assocList Id.from (Column.decoder clientHeight)) <|
        \dictWithCmds ->
            let
                reducer cId ( c, initCmd ) ( accDict, accCmds ) =
                    Tuple.pair (Dict.insert cId c accDict) <|
                        if Array.member cId order then
                            ( cId, initCmd ) :: accCmds

                        else
                            accCmds
            in
            D.succeed (Dict.foldl reducer ( Dict.empty, [] ) dictWithCmds)


encode : ColumnStore -> Storable
encode columnStore =
    Data.Storable.encode storeId
        [ ( "dict", E.assocList Id.to Column.encode columnStore.dict )
        , ( "order", E.array (E.string << Id.to) columnStore.order )
        , ( "fam", FAM.encode columnStore.fam )
        ]


storeId : String
storeId =
    "columnStore"


init : Int -> ColumnStore
init posix =
    ColumnStore Dict.empty Array.empty FAM.init Deque.empty (Random.initialSeed posix)


size : ColumnStore -> Int
size cs =
    Dict.size cs.dict


sizePinned : ColumnStore -> Int
sizePinned cs =
    let
        countPinned _ c acc =
            if Column.getPinned c then
                acc + 1

            else
                acc
    in
    Dict.foldl countPinned 0 cs.dict



-- SINGULAR APIs


add : Maybe Int -> Column -> ColumnStore -> ColumnStore
add limitMaybe c columnStore =
    let
        newDict =
            Dict.insert (Column.getId c) c columnStore.dict

        newOrder =
            columnStore.order |> Array.squeeze 0 (Column.getId c) |> autoArrange limitMaybe newDict

        newScanQueue =
            -- Previous relative scan ordering is kept
            Deque.pushFront (Column.getId c) columnStore.scanQueue
    in
    { columnStore | dict = newDict, order = newOrder, scanQueue = newScanQueue }


get : Int -> ColumnStore -> Maybe Column
get index columnStore =
    columnStore.order
        |> Array.get index
        |> Maybe.andThen (\id -> Dict.get id columnStore.dict)


remove : Column.Id -> ColumnStore -> ColumnStore
remove cId columnStore =
    let
        cs_ =
            case Array.findIndex ((==) cId) columnStore.order of
                Just index ->
                    { columnStore | order = Array.removeAt index columnStore.order }

                Nothing ->
                    columnStore

        newDict =
            Dict.remove cId columnStore.dict
    in
    -- Discard previous scan ordering
    { cs_ | dict = newDict, scanQueue = Deque.fromList (Dict.keys newDict) }


touchAt : Int -> ColumnStore -> ColumnStore
touchAt index columnStore =
    case get index columnStore of
        Just c ->
            { columnStore | dict = Dict.insert (Column.getId c) (Column.setRecentlyTouched True c) columnStore.dict }

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


mapForViewImpl : (Int -> Column -> a) -> Dict Column.Id Column -> List Column.Id -> Int -> List a -> List a
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
            if Array.member cId columnStore.order then
                acc

            else
                c :: acc
    in
    Dict.foldr reducer [] columnStore.dict
        -- Sort is necessary, since AssocList is internally shuffled (due to remove then cons) on insert
        |> List.sortBy (Column.getId >> Id.to)



-- Component APIs


updateById : Maybe Int -> Column.Id -> Column.Msg -> ColumnStore -> ( ColumnStore, Column.PostProcess )
updateById limitMaybe cId cMsg columnStore =
    case Dict.get cId columnStore.dict of
        Just c ->
            let
                isVisible =
                    Array.member (Column.getId c) columnStore.order

                ( newC, pp ) =
                    Column.update isVisible cMsg c

                newDict =
                    Dict.insert cId newC columnStore.dict

                newOrder =
                    case pp.position of
                        Auto ->
                            autoArrange limitMaybe newDict columnStore.order

                        Bump ->
                            if isVisible then
                                -- Already visible columns should not be reordered abruptly, either pinned/loose.
                                -- Rather we should notify users via e.g. badge on sidebar?
                                columnStore.order

                            else
                                columnStore.order |> Array.squeeze 0 (Column.getId c) |> autoArrange limitMaybe newDict

                        Keep ->
                            columnStore.order
            in
            ( { columnStore | dict = newDict, order = newOrder }, pp )

        Nothing ->
            pure columnStore


pure : ColumnStore -> ( ColumnStore, Column.PostProcess )
pure columnStore =
    ( columnStore, Column.postProcess )


autoArrange : Maybe Int -> Dict Column.Id Column -> Array Column.Id -> Array Column.Id
autoArrange limitMaybe dict order =
    let
        stableClassify cId ( accPinned, accLoose ) =
            case Dict.get cId dict of
                Just c ->
                    if Column.getPinned c then
                        ( Array.push cId accPinned, accLoose )

                    else
                        ( accPinned, Array.push cId accLoose )

                Nothing ->
                    -- Should not happen
                    ( accPinned, accLoose )

        concatClassified ( pinned, loose ) =
            case limitMaybe of
                Just limit ->
                    let
                        looseLimit =
                            max 0 (limit - Array.length pinned)
                    in
                    Array.append pinned (Array.slice 0 looseLimit loose)

                Nothing ->
                    Array.append pinned loose
    in
    order
        |> Array.foldl stableClassify ( Array.empty, Array.empty )
        |> concatClassified


applyOrder : Array Column.Id -> ColumnStore -> ColumnStore
applyOrder order columnStore =
    { columnStore | order = order }


consumeBroker :
    Maybe Int
    -> { broker : Broker Item, maxCount : Int, clientHeight : Int, catchUp : Bool }
    -> ColumnStore
    -> ( ColumnStore, Maybe ( Column.Id, Column.PostProcess ) )
consumeBroker limitMaybe opts columnStore =
    case Deque.popBack columnStore.scanQueue of
        ( Just cId, newScanQueue ) ->
            updateById limitMaybe cId (Column.ScanBroker opts) { columnStore | scanQueue = Deque.pushFront cId newScanQueue }
                |> Tuple.mapSecond (Just << Tuple.pair cId)

        ( Nothing, _ ) ->
            ( columnStore, Nothing )


updateFAM : List UpdateInstruction -> ColumnStore -> ( ColumnStore, Bool )
updateFAM instructions columnStore =
    FAM.update instructions columnStore.fam
        |> Tuple.mapFirst (\newFAM -> { columnStore | fam = newFAM })
