module Data.ColumnStore exposing
    ( ColumnStore, SwapState, init, addWelcome, encode, decoder, storeId, size, sizePinned
    , map, listVisible, listShadow
    , Msg(..), PostProcess, update, updateFAM
    , setDict, setOrder, setSwapState, setFam, setAvailableSources, setScanQueue, setSeed
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

@docs ColumnStore, SwapState, init, addWelcome, encode, decoder, storeId, size, sizePinned
@docs map, listVisible, listShadow
@docs Msg, PostProcess, update, updateFAM
@docs setDict, setOrder, setSwapState, setFam, setAvailableSources, setScanQueue, setSeed

-}

import Array exposing (Array)
import ArrayExtra as Array
import AssocList as Dict exposing (Dict)
import Browser.Dom
import Data.Column as Column exposing (Column, Position(..))
import Data.ColumnStore.AvailableSources as AvailableSources exposing (AvailableSources)
import Data.Filter exposing (FilterAtom(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial, UpdateInstruction)
import Data.ProducerRegistry as ProducerRegistry
import Data.Storable exposing (Storable)
import Deque exposing (Deque)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Random
import Task exposing (Task)
import View.Templates.Main exposing (columnAreaParentId, columnWidth)


type alias ColumnStore =
    { dict : Dict Column.Id Column
    , order : Array Column.Id
    , swapState : Maybe SwapState
    , fam : FilterAtomMaterial
    , availableSources : AvailableSources
    , scanQueue : Deque Column.Id
    , seed : Random.Seed
    }


type alias SwapState =
    { grabbedId : Column.Id
    , pinned : Bool
    , originalIndex : Int
    , originalOrder : Array Column.Id
    }


init : Int -> ColumnStore
init posix =
    { dict = Dict.empty
    , order = Array.empty
    , swapState = Nothing
    , fam = FAM.init
    , availableSources = AvailableSources.init
    , scanQueue = Deque.empty
    , seed = Random.initialSeed posix
    }


decoder : { clientHeight : Int, posix : Int } -> Decoder ( ColumnStore, Cmd Msg )
decoder { clientHeight, posix } =
    D.do (D.field "order" (D.array (Id.decoder D.string))) <|
        \order ->
            D.do (D.field "fam" FAM.decoder) <|
                \fam ->
                    D.do (D.field "dict" (dictAndInitCmdDecoder clientHeight order fam)) <|
                        \( dict, cmds ) ->
                            init posix
                                |> setDict dict
                                |> setFam fam
                                |> setOrder order
                                |> setScanQueue (Deque.fromList (Dict.keys dict))
                                |> D.succeed
                                |> D.map2 setAvailableSources (availableSourcesDecoder fam)
                                |> D.map (\cs -> ( cs, Cmd.batch cmds ))


dictAndInitCmdDecoder : Int -> Array Column.Id -> FilterAtomMaterial -> Decoder ( Dict Column.Id Column, List (Cmd Msg) )
dictAndInitCmdDecoder clientHeight order fam =
    D.do (D.assocList Id.from (Column.decoder clientHeight fam)) <|
        \dictWithCmds ->
            let
                reducer cId ( c, initCmd ) ( accDict, accCmds ) =
                    Tuple.pair (Dict.insert cId c accDict) <|
                        if Array.member cId order then
                            Cmd.map (ById cId) initCmd :: accCmds

                        else
                            accCmds
            in
            D.succeed (Dict.foldl reducer ( Dict.empty, [] ) dictWithCmds)


availableSourcesDecoder : FilterAtomMaterial -> Decoder AvailableSources
availableSourcesDecoder { ofDiscordChannel, ofSlackConversation } =
    D.oneOf
        [ D.field "availableSources" AvailableSources.decoder
        , D.lazy <|
            \() ->
                let
                    discordChannels =
                        case ofDiscordChannel of
                            Just ( _, channelCaches ) ->
                                channelCaches

                            Nothing ->
                                []

                    slackConvos =
                        case ofSlackConversation of
                            Just { convos } ->
                                convos

                            Nothing ->
                                []
                in
                D.succeed (AvailableSources discordChannels slackConvos)
        ]


encode : ColumnStore -> Storable
encode cs =
    Data.Storable.encode storeId
        [ ( "dict", E.assocList Id.to Column.encode cs.dict )
        , ( "order", E.array (E.string << Id.to) cs.order )
        , ( "fam", FAM.encode cs.fam )

        -- TODO store availableSources
        ]


storeId : String
storeId =
    "columnStore"


{-| Add a welcome column to the ColumnStore. Exposed for Model initialization.
-}
addWelcome : Int -> ColumnStore -> ColumnStore
addWelcome clientHeight cs =
    let
        ( c, seed ) =
            Random.step (Column.welcomeGenerator clientHeight) cs.seed
    in
    add Nothing c { cs | seed = seed }


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



-- BULK APIs


{-| Map over all Columns in ColumnStore, either visible or shadow.
-}
map : (Column -> Column) -> ColumnStore -> ColumnStore
map mapper cs =
    { cs | dict = Dict.map (\_ c -> mapper c) cs.dict }


listVisible : ColumnStore -> List Column
listVisible cs =
    List.filterMap (\id -> Dict.get id cs.dict) (Array.toList cs.order)


listShadow : ColumnStore -> List Column
listShadow cs =
    let
        reducer cId c acc =
            if Array.member cId cs.order then
                acc

            else
                c :: acc
    in
    Dict.foldr reducer [] cs.dict
        -- Sort is necessary, since AssocList is internally shuffled (due to remove then cons) on insert
        |> List.sortBy (Column.getId >> Id.to)



-- Component APIs


type Msg
    = AddEmpty Int
    | AddSimple Int FilterAtom
    | Delete Column.Id
    | Dismiss Int
    | Reveal Int
    | SwapStart { index : Int, id : Column.Id, pinned : Bool }
    | ApplyOrder (Array Column.Id)
    | SwapEnd
    | ConsumeBroker Column.ScanOptions
    | ById Column.Id Column.Msg
    | NoOp


type alias PostProcess =
    { cmd : Cmd Msg
    , persist : Bool
    , catchUpId : Maybe Column.Id
    , producerMsg : Maybe ProducerRegistry.Msg
    }


postProcess : PostProcess
postProcess =
    { cmd = Cmd.none
    , persist = False
    , catchUpId = Nothing
    , producerMsg = Nothing
    }


update : Maybe Int -> Msg -> ColumnStore -> ( ColumnStore, PostProcess )
update limitMaybe msg cs =
    case msg of
        AddEmpty clientHeight ->
            let
                ( c, seed ) =
                    Random.step (Column.emptyGenerator clientHeight) cs.seed
            in
            -- If Filters are somehow set to the new Column, then persist.
            pure (add limitMaybe c { cs | seed = seed })

        AddSimple clientHeight filterAtom ->
            let
                ( c, seed ) =
                    Random.step (Column.simpleGenerator clientHeight filterAtom) cs.seed
            in
            ( add limitMaybe c { cs | seed = seed }
            , { postProcess | persist = True, catchUpId = Just (Column.getId c) }
            )

        Delete id ->
            let
                cs_ =
                    case Array.findIndex ((==) id) cs.order of
                        Just index ->
                            { cs | order = Array.removeAt index cs.order }

                        Nothing ->
                            cs

                newDict =
                    Dict.remove id cs.dict
            in
            -- Discard previous scan ordering
            ( { cs_ | dict = newDict, scanQueue = Deque.fromList (Dict.keys newDict) }
            , { postProcess | persist = True }
            )

        Dismiss index ->
            ( { cs | order = Array.removeAt index cs.order }, { postProcess | persist = True } )

        Reveal index ->
            case Array.get index cs.order of
                Just cId ->
                    ( { cs | dict = Dict.update cId (Maybe.map (Column.setRecentlyTouched True)) cs.dict }
                    , { postProcess | cmd = reveal index }
                    )

                Nothing ->
                    -- Should not happen
                    pure cs

        SwapStart { index, pinned, id } ->
            pure { cs | swapState = Just (SwapState id pinned index cs.order) }

        ApplyOrder newOrder ->
            -- XXX Why don't we take newIndex and apply ArrayExtra.moveFromTo here?
            -- Well it is because it allows "illegal messages" to be represented!
            -- If we take newIndex, it must be applied onto originalOrder here,
            -- but ***originalOrder only exists when swapState /= Nothing!***
            -- To eliminate such check, we have to make sure ApplyOrder is fired only when swapState exists.
            -- However, such guarantee can only be realized in view functons!
            -- Also see src/View/Pages/Main.elm
            pure { cs | order = newOrder }

        SwapEnd ->
            -- Drop event is somewhat flaky to correctly track, so we should always turn off swap at Dragend
            ( { cs | swapState = Nothing }, { postProcess | persist = True } )

        ConsumeBroker opts ->
            case Deque.popBack cs.scanQueue of
                ( Just cId, newScanQueue ) ->
                    updateById limitMaybe cId (Column.ScanBroker opts) { cs | scanQueue = Deque.pushFront cId newScanQueue }

                ( Nothing, _ ) ->
                    -- No columns are subscribing to the Broker, nothing to do.
                    pure cs

        ById id cMsg ->
            updateById limitMaybe id cMsg cs

        NoOp ->
            pure cs


add : Maybe Int -> Column -> ColumnStore -> ColumnStore
add limitMaybe c cs =
    let
        newDict =
            Dict.insert (Column.getId c) c cs.dict

        newOrder =
            cs.order |> Array.squeeze 0 (Column.getId c) |> autoArrange limitMaybe newDict

        newScanQueue =
            -- Previous relative scan ordering is kept
            Deque.pushFront (Column.getId c) cs.scanQueue
    in
    { cs | dict = newDict, order = newOrder, scanQueue = newScanQueue }


reveal : Int -> Cmd Msg
reveal index =
    Browser.Dom.getViewportOf columnAreaParentId
        |> Task.andThen (scrollToColumn index)
        |> Task.attempt (always NoOp)


scrollToColumn : Int -> Browser.Dom.Viewport -> Task Browser.Dom.Error ()
scrollToColumn index parentVp =
    let
        cWidth =
            toFloat columnWidth

        targetX =
            cWidth * toFloat index
    in
    if targetX < parentVp.viewport.x then
        Browser.Dom.setViewportOf columnAreaParentId targetX 0

    else if targetX + cWidth < parentVp.viewport.x + parentVp.viewport.width then
        Task.succeed ()

    else
        Browser.Dom.setViewportOf columnAreaParentId (targetX + cWidth - parentVp.viewport.width) 0


updateById : Maybe Int -> Column.Id -> Column.Msg -> ColumnStore -> ( ColumnStore, PostProcess )
updateById limitMaybe cId cMsg cs =
    case Dict.get cId cs.dict of
        Just c ->
            let
                isVisible =
                    Array.member cId cs.order

                ( newC, cPostProcess ) =
                    Column.update isVisible cMsg c

                newDict =
                    Dict.insert cId newC cs.dict

                newOrder =
                    case cPostProcess.position of
                        Auto ->
                            autoArrange limitMaybe newDict cs.order

                        Bump ->
                            if isVisible then
                                -- Already visible columns should not be reordered abruptly, either pinned or loose.
                                -- Rather we should notify users via e.g. badge on sidebar?
                                cs.order

                            else
                                autoArrange limitMaybe newDict (Array.squeeze 0 cId cs.order)

                        Keep ->
                            cs.order
            in
            ( { cs | dict = newDict, order = newOrder }
            , { cmd = Cmd.map (ById cId) cPostProcess.cmd
              , persist = cPostProcess.persist
              , catchUpId = cPostProcess.catchUpId
              , producerMsg = cPostProcess.producerMsg
              }
            )

        Nothing ->
            pure cs


pure : ColumnStore -> ( ColumnStore, PostProcess )
pure cs =
    ( cs, postProcess )


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


updateFAM : List UpdateInstruction -> ColumnStore -> ( ColumnStore, Bool )
updateFAM instructions cs =
    FAM.update instructions cs.fam
        |> Tuple.mapFirst (\newFAM -> { cs | fam = newFAM })



-- Accessors


setDict : Dict Column.Id Column -> ColumnStore -> ColumnStore
setDict val cs =
    { cs | dict = val }


setOrder : Array Column.Id -> ColumnStore -> ColumnStore
setOrder val cs =
    { cs | order = val }


setSwapState : Maybe SwapState -> ColumnStore -> ColumnStore
setSwapState val cs =
    { cs | swapState = val }


setFam : FilterAtomMaterial -> ColumnStore -> ColumnStore
setFam val cs =
    { cs | fam = val }


setAvailableSources : AvailableSources -> ColumnStore -> ColumnStore
setAvailableSources val cs =
    { cs | availableSources = val }


setScanQueue : Deque Column.Id -> ColumnStore -> ColumnStore
setScanQueue val cs =
    { cs | scanQueue = val }


setSeed : Random.Seed -> ColumnStore -> ColumnStore
setSeed val cs =
    { cs | seed = val }
