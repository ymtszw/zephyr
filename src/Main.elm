module Main exposing (main)

import Array
import ArrayExtra as Array
import Broker exposing (Broker)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.FilterAtomMaterial as FilterAtomMaterial
import Data.ItemBroker as ItemBroker
import Data.Model as Model exposing (ColumnSwap, Env, Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.UniqueId as UniqueId
import Extra exposing (..)
import IndexedDb
import Json.Decode as D
import Json.DecodeExtra as D
import Logger
import Task
import Time
import TimeZone
import Url
import View
import View.Select



-- MAIN


main : Program Env Model Msg
main =
    Browser.application
        { init = init
        , update = \msg m -> log update msg m |> IndexedDb.postUpdate
        , subscriptions = sub
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = \_ -> NoOp
        }


log : (Msg -> Model -> ( Model, Cmd Msg, Bool )) -> Msg -> Model -> ( Model, Cmd Msg, Bool )
log u msg m =
    u msg <|
        if m.env.isLocalDevelopment then
            Logger.push m.idGen (Data.Msg.logEntry msg) m.log
                |> (\( newLog, idGen ) -> { m | log = newLog, idGen = idGen })

        else
            m



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    Model.init env navKey
        |> andDo
            [ adjustMaxHeight
            , getTimeZone

            -- , ite env.indexedDBAvailable Cmd.none scheduleNextScan -- Wait scanning until Load
            ]


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport Browser.Dom.getViewport


getTimeZone : Cmd Msg
getTimeZone =
    let
        fallbackToUtc =
            Result.withDefault ( "UTC", Time.utc ) >> GetTimeZone
    in
    TimeZone.getZone |> Task.attempt fallbackToUtc


scheduleNextScan : Cmd Msg
scheduleNextScan =
    -- Not using Time.every subscription since Column updating may take longer time occasionally,
    -- in such events fixed ticks may interfare with the adjacent ticks.
    -- Chained timers can always ensure next processing is AFTER the previous one had done.
    setTimeout ScanBroker scanIntervalMillis


{-| Dictates how often Columns are updated (i.e. the Broker is scanned).

XXX We may have to dynamically adjust scanIntervalMillis and brokerScanChunkAmount
according to the flow rate of the Broker. Or, instruct users about how to configure Columns so they are not overloaded.
Since it is possible for Producers to produce Items way faster than Consumers (Columns) can consume.

Due to the "buffer" nature of the Broker, the application itself should continue to run,
though older Items may be evicted BEFORE consumed by Columns, effectively causing "skip" or "loss" of data.

-}
scanIntervalMillis : Float
scanIntervalMillis =
    1000



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg ({ viewState, env } as m) =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            ( m, adjustMaxHeight, False )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars
            pure { m | env = { env | clientHeight = round viewport.height } }

        GetTimeZone ( _, zone ) ->
            pure { m | viewState = { viewState | timezone = zone } }

        LoggerCtrl lMsg ->
            Logger.update lMsg m.log |> Tuple.mapBoth (\l -> { m | log = l }) (Cmd.map LoggerCtrl) |> IndexedDb.noPersist

        LinkClicked (Browser.Internal url) ->
            ( m, Nav.pushUrl m.navKey (Url.toString url), False )

        LinkClicked (Browser.External url) ->
            ( m, Nav.load url, False )

        SelectToggle sId True ->
            pure { m | viewState = { viewState | selectState = View.Select.open sId viewState.selectState } }

        SelectToggle _ False ->
            pure { m | viewState = { viewState | selectState = View.Select.close } }

        SelectPick actualMsg ->
            update actualMsg { m | viewState = { viewState | selectState = View.Select.close } }

        AddColumn ->
            -- If Filters are somehow set to the new Column, then persist.
            pure (addColumn m)

        DelColumn index ->
            ( { m | columnStore = ColumnStore.removeAt index m.columnStore }, Cmd.none, True )

        ToggleColumnSwappable True ->
            pure { m | viewState = { viewState | columnSwappable = True } }

        ToggleColumnSwappable False ->
            -- DragEnd may get lost. Fix zombie drag here.
            pure { m | viewState = { viewState | columnSwappable = False, columnSwapMaybe = Nothing } }

        DragStart originalIndex colId ->
            pure { m | viewState = { viewState | columnSwapMaybe = Just (ColumnSwap colId originalIndex m.columnStore.order) } }

        DragEnter dest ->
            pure (onDragEnter m dest)

        DragEnd ->
            -- During HTML5 drag, KeyboardEvent won't fire (modifier key situations are accessible via DragEvent though).
            -- So we always turn off swap mode at dragend
            pure { m | viewState = { viewState | columnSwappable = False, columnSwapMaybe = Nothing } }

        LoadOk ss ->
            -- TODO break them apart; save/load one gigantic state object is one of anti-pattern
            reloadProducers <|
                { m
                    | columnStore = ss.columnStore
                    , itemBroker = ss.itemBroker
                    , producerRegistry = ss.producerRegistry
                    , idGen = ss.idGen
                }

        LoadErr _ ->
            pure m

        ToggleConfig opened ->
            pure { m | viewState = { viewState | configOpen = opened } }

        ToggleColumnConfig cId bool ->
            updateColumn cId m False <| \c -> { c | configOpen = bool, deleteGate = "" }

        AddColumnFilter cId filter ->
            updateColumn cId m True <| \c -> { c | filters = Array.push filter c.filters, offset = Nothing, items = [] }

        SetColumnFilter cId index filter ->
            updateColumn cId m True <| \c -> { c | filters = Array.set index filter c.filters, offset = Nothing, items = [] }

        DelColumnFilter cId index ->
            updateColumn cId m True <| \c -> { c | filters = Array.removeAt index c.filters, offset = Nothing, items = [] }

        ColumnDeleteGateInput cId text ->
            -- Bypassing unrelated checks in updateColumn
            pure { m | columnStore = ColumnStore.updateById cId (\c -> { c | deleteGate = text }) m.columnStore }

        ProducerCtrl pctrl ->
            applyProducerYield m <| Producer.update pctrl m.producerRegistry

        ScanBroker _ ->
            scanBroker m

        NoOp ->
            pure m


addColumn : Model -> Model
addColumn m =
    let
        ( newColumn, newIdGen ) =
            m.idGen
                |> UniqueId.gen "column"
                |> UniqueId.andThen (\( cId, idGen ) -> Column.new idGen cId)
    in
    { m | columnStore = ColumnStore.add newColumn m.columnStore, idGen = newIdGen }


onDragEnter : Model -> Int -> Model
onDragEnter m dest =
    -- Ideally we should pass originalOrder Array along with messages
    -- so that this case clause can be eliminated. ("Make impossible states unrepresentable.")
    -- However currently there is a bug that prevents --debug compilation
    -- when Arrays are passed in messages. See https://github.com/elm/compiler/issues/1753
    case m.viewState.columnSwapMaybe of
        Just swap ->
            let
                newOrder =
                    Array.moveFromTo swap.originalIndex dest swap.originalOrder
            in
            { m | columnStore = ColumnStore.applyOrder newOrder m.columnStore }

        Nothing ->
            m


{-| Update column somehow, someway.

Can choose whether to persist or not.

-}
updateColumn : String -> Model -> Bool -> (Column -> Column) -> ( Model, Cmd Msg, Bool )
updateColumn cId m persistRequested updater =
    let
        ( newModel, shouldPersist ) =
            { m | columnStore = ColumnStore.updateById cId updater m.columnStore }
                |> updateProducerFetchStatuses
                |> Tuple.mapSecond ((||) persistRequested)
    in
    ( newModel, Cmd.none, shouldPersist )


scanBroker : Model -> ( Model, Cmd Msg, Bool )
scanBroker m =
    let
        ( newColumnStore, shouldPersist ) =
            ColumnStore.consumeBroker brokerScanChunkAmount m.itemBroker m.columnStore
    in
    ( { m | columnStore = newColumnStore }, scheduleNextScan, shouldPersist )


brokerScanChunkAmount : Int
brokerScanChunkAmount =
    500


updateProducerFetchStatuses : Model -> ( Model, Bool )
updateProducerFetchStatuses ({ producerRegistry } as m) =
    -- This function should also "fix" corrupted Producer statuses, if any.
    let
        ( newDiscord, shouldPersist ) =
            case producerRegistry.discord of
                Just discord ->
                    Discord.setChannelFetchStatus (ColumnStore.discordChannelIds m.columnStore) discord
                        |> Tuple.mapFirst Just

                Nothing ->
                    ( Nothing, False )
    in
    ( { m | producerRegistry = { producerRegistry | discord = newDiscord } }, shouldPersist )



-- PRODUCER


{-| Relstart producers on savedState reload.

Always persist state in order to apply new encoding format, if any.

-}
reloadProducers : Model -> ( Model, Cmd Msg, Bool )
reloadProducers ({ viewState } as m) =
    let
        ( newRegistry, reloadCmd, famInstruction ) =
            Producer.reloadAll m.producerRegistry
    in
    ( { m
        | producerRegistry = newRegistry
        , viewState = { viewState | filterAtomMaterial = FilterAtomMaterial.update famInstruction viewState.filterAtomMaterial }
      }
    , Cmd.batch
        [ Cmd.map ProducerCtrl reloadCmd
        , ite m.env.indexedDBAvailable scheduleNextScan Cmd.none
        ]
    , True
    )


applyProducerYield : Model -> Producer.GrossYield -> ( Model, Cmd Msg, Bool )
applyProducerYield ({ viewState } as m) gy =
    case gy.items of
        [] ->
            ( { m
                | producerRegistry = gy.producerRegistry
                , viewState =
                    { viewState
                        | filterAtomMaterial =
                            FilterAtomMaterial.update gy.postProcess.famInstruction viewState.filterAtomMaterial
                    }
              }
            , Cmd.map ProducerCtrl gy.cmd
            , gy.postProcess.persist
            )

        nonEmptyYields ->
            ( { m
                | itemBroker = ItemBroker.bulkAppend nonEmptyYields m.itemBroker
                , producerRegistry = gy.producerRegistry
                , viewState =
                    { viewState
                        | filterAtomMaterial =
                            FilterAtomMaterial.update gy.postProcess.famInstruction viewState.filterAtomMaterial
                    }
              }
            , Cmd.map ProducerCtrl gy.cmd
            , gy.postProcess.persist
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ Browser.Events.onResize Resize
        , IndexedDb.load m.idGen
        , toggleColumnSwap m.viewState.columnSwappable
        ]


toggleColumnSwap : Bool -> Sub Msg
toggleColumnSwap swappable =
    Sub.batch
        [ if not swappable then
            Browser.Events.onKeyDown (D.when (D.field "altKey" D.bool) identity (D.succeed (ToggleColumnSwappable True)))

          else
            Browser.Events.onKeyUp (D.when (D.field "altKey" D.bool) not (D.succeed (ToggleColumnSwappable False)))
        , Browser.Events.onVisibilityChange <|
            \visibility ->
                case visibility of
                    Browser.Events.Visible ->
                        NoOp

                    Browser.Events.Hidden ->
                        ToggleColumnSwappable False
        ]



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.body m
    }
