module Main exposing (main)

import Array
import ArrayExtra as Array
import Broker exposing (Broker)
import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (Visibility(..), onResize)
import Browser.Navigation as Nav exposing (Key)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter as Filter
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Model as Model exposing (ColumnSwap, Env, Model, welcomeModel)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.UniqueId as UniqueId
import Extra exposing (andDo, ite, pure, setTimeout)
import HttpExtra
import IndexedDb
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Logger exposing (Entry)
import String exposing (fromInt)
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



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    Model.init env navKey
        |> andDo
            [ adjustMaxHeight
            , getTimeZone
            , ite env.indexedDBAvailable Cmd.none scheduleNextScan -- Wait scanning until Load
            ]


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport getViewport


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

        LinkClicked (Internal url) ->
            ( m, Nav.pushUrl m.navKey (Url.toString url), False )

        LinkClicked (External url) ->
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

        ToggleColumnSwappable bool ->
            pure { m | viewState = { viewState | columnSwappable = bool } }

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
            updateColumn cId m False <| \c -> { c | deleteGate = text }

        ProducerCtrl pctrl ->
            applyProducerYield m <| Producer.update pctrl m.producerRegistry

        ScanBroker _ ->
            scanBroker m

        NoOp ->
            pure m


addColumn : Model -> Model
addColumn m =
    let
        ( newId, newIdGen ) =
            UniqueId.gen "column" m.idGen
    in
    { m | columnStore = ColumnStore.add (Column.new newId) m.columnStore, idGen = newIdGen }


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
    -- This function should "fix" corrupted Producer statuses, if any.
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

Save state in order to apply new encoding format.

-}
reloadProducers : Model -> ( Model, Cmd Msg, Bool )
reloadProducers m =
    let
        ( newRegistry, reloadCmd ) =
            Producer.reloadAll m.producerRegistry
    in
    ( { m | producerRegistry = newRegistry }
    , Cmd.batch
        [ Cmd.map ProducerCtrl reloadCmd
        , ite m.env.indexedDBAvailable scheduleNextScan Cmd.none
        ]
    , True
    )


applyProducerYield : Model -> Producer.GrossYield -> ( Model, Cmd Msg, Bool )
applyProducerYield model gy =
    case gy.items of
        [] ->
            ( { model | producerRegistry = gy.producerRegistry }
            , Cmd.map ProducerCtrl gy.cmd
            , gy.shouldPersist
            )

        nonEmptyYields ->
            ( { model
                | itemBroker = ItemBroker.bulkAppend nonEmptyYields model.itemBroker
                , producerRegistry = gy.producerRegistry
              }
            , Cmd.map ProducerCtrl gy.cmd
            , gy.shouldPersist
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ onResize Resize
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
                    Visible ->
                        NoOp

                    Hidden ->
                        ToggleColumnSwappable False
        ]



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.body m
    }



-- LOGGING


log : (Msg -> Model -> ( Model, Cmd Msg, Bool )) -> Msg -> Model -> ( Model, Cmd Msg, Bool )
log u msg m =
    u msg <|
        if m.env.isLocalDevelopment then
            { m | log = Logger.rec m.log (msgToLogEntry msg) }

        else
            m


msgToLogEntry : Msg -> Entry
msgToLogEntry msg =
    case msg of
        NoOp ->
            Entry "NoOp" []

        Resize x y ->
            Entry "Resize" [ fromInt x, fromInt y ]

        GetViewport vp ->
            Entry "GetViewport" [ viewportToString vp ]

        GetTimeZone ( name, _ ) ->
            Entry "GetTimeZone" [ name ]

        LoggerCtrl lMsg ->
            loggerMsgToEntry lMsg

        LinkClicked (Internal url) ->
            Entry "LinkClicked.Internal" [ Url.toString url ]

        LinkClicked (External str) ->
            Entry "LinkClicked.External" [ str ]

        SelectToggle sId bool ->
            Entry "SelectToggle" [ sId, ite bool "True" "False" ]

        SelectPick sMsg ->
            msgToLogEntry sMsg

        AddColumn ->
            Entry "AddColumn" []

        DelColumn index ->
            Entry "DelColumn" [ fromInt index ]

        ToggleColumnSwappable bool ->
            Entry "ToggleColumnSwappable" [ ite bool "True" "False" ]

        DragStart index cId ->
            Entry "DragStart" [ fromInt index, cId ]

        DragEnter index ->
            Entry "DragEnter" [ fromInt index ]

        DragEnd ->
            Entry "DragEnd" []

        LoadOk _ ->
            Entry "LoadOk" [ "<savedState>" ]

        LoadErr e ->
            Entry "LoadErr" [ D.errorToString e ]

        ToggleConfig bool ->
            Entry "ToggleConfig" [ ite bool "True" "False" ]

        ToggleColumnConfig cId bool ->
            Entry "ToggleColumnConfig" [ cId, ite bool "True" "False" ]

        AddColumnFilter cId filter ->
            Entry "AddColumnFilter" [ cId, Filter.toString filter ]

        SetColumnFilter cId index filter ->
            Entry "SetColumnFilter" [ cId, fromInt index, Filter.toString filter ]

        DelColumnFilter cId index ->
            Entry "DelColumnFilter" [ cId, fromInt index ]

        ColumnDeleteGateInput cId input ->
            Entry "ColumnDeleteGateInput" [ cId, input ]

        ProducerCtrl pMsg ->
            producerMsgToEntry pMsg

        ScanBroker posix ->
            Entry "ScanBroker" [ Iso8601.fromTime posix ]


viewportToString : Viewport -> String
viewportToString vp =
    E.encode 2 <|
        E.object
            [ Tuple.pair "scene" <|
                E.object
                    [ ( "width", E.float vp.scene.width )
                    , ( "height", E.float vp.scene.height )
                    ]
            , Tuple.pair "viewport" <|
                E.object
                    [ ( "width", E.float vp.viewport.width )
                    , ( "height", E.float vp.viewport.height )
                    , ( "x", E.float vp.viewport.x )
                    , ( "y", E.float vp.viewport.y )
                    ]
            ]


loggerMsgToEntry : Logger.Msg -> Entry
loggerMsgToEntry lMsg =
    case lMsg of
        Logger.ScrollStart ->
            Entry "Logger.ScrollStart" []

        Logger.BackToTop ->
            Entry "Logger.BackToTop" []

        Logger.ViewportResult (Ok ( _, vp )) ->
            Entry "Logger.ViewportOk" [ viewportToString vp ]

        Logger.ViewportResult (Err _) ->
            Entry "Logger.ViewportNotFound" []

        Logger.FilterInput query ->
            Entry "Logger.FilterInput" [ query ]

        Logger.SetMsgFilter (Logger.MsgFilter isPos ctor) ->
            Entry "Logger.SetMsgFilter" [ ite isPos "Include: " "Exclude: " ++ ctor ]

        Logger.DelMsgFilter (Logger.MsgFilter isPos ctor) ->
            Entry "Logger.DelMsgFilter" [ ite isPos "Include: " "Exclude: " ++ ctor ]

        Logger.NoOp ->
            Entry "Logger.NoOp" []


producerMsgToEntry : Producer.Msg -> Entry
producerMsgToEntry pMsg =
    case pMsg of
        Producer.DiscordMsg msgDiscord ->
            case msgDiscord of
                Discord.TokenInput input ->
                    Entry "Discord.TokenInput" [ input ]

                Discord.CommitToken ->
                    Entry "Discord.CommitToken" []

                Discord.Identify user ->
                    Entry "Discord.Identify" [ E.encode 2 (Discord.encodeUser user) ]

                Discord.Hydrate _ _ ->
                    Entry "Discord.Hydrate" [ "<Hydrate>" ]

                Discord.Rehydrate ->
                    Entry "Discord.Rehydrate" []

                Discord.Fetch posix ->
                    Entry "Discord.Fetch" [ Iso8601.fromTime posix ]

                Discord.Fetched (Discord.FetchOk cId ms posix) ->
                    Entry "Discord.FetchOk" [ cId, Iso8601.fromTime posix, E.encode 2 (E.list Discord.encodeMessage ms) ]

                Discord.Fetched (Discord.FetchErr cId e) ->
                    Entry "Discord.FetchErr" [ cId, HttpExtra.errorToString e ]

                Discord.APIError e ->
                    Entry "Discord.APIError" [ HttpExtra.errorToString e ]
