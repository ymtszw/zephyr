module Main exposing (main)

import Array
import ArrayExtra as Array
import Broker exposing (Broker)
import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (getViewport)
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
import Extra exposing (ite, setTimeout)
import HttpExtra
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Logger exposing (Entry)
import Ports
import String exposing (fromInt)
import Task
import Url
import View
import View.Select



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    Model.init env navKey
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, adjustMaxHeight, scheduleNextScan ])


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport getViewport


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
    500



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ viewState, env } as m) =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            ( m, adjustMaxHeight )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars
            ( { m | env = { env | clientHeight = round viewport.height } }, Cmd.none )

        LinkClicked (Internal url) ->
            ( m, Nav.pushUrl m.navKey (Url.toString url) )

        LinkClicked (External url) ->
            ( m, Nav.load url )

        SelectToggle sId True ->
            ( { m | viewState = { viewState | selectState = View.Select.open sId viewState.selectState } }, Cmd.none )

        SelectToggle _ False ->
            ( { m | viewState = { viewState | selectState = View.Select.close } }, Cmd.none )

        SelectPick actualMsg ->
            update actualMsg { m | viewState = { viewState | selectState = View.Select.close } }

        AddColumn ->
            persist ( addColumn m, Cmd.none )

        DelColumn index ->
            persist ( { m | columnStore = ColumnStore.removeAt index m.columnStore }, Cmd.none )

        ToggleColumnSwappable bool ->
            ( { m | viewState = { viewState | columnSwappable = bool } }, Cmd.none )

        DragStart originalIndex colId ->
            ( { m | viewState = { viewState | columnSwapMaybe = Just (ColumnSwap colId originalIndex m.columnStore.order) } }, Cmd.none )

        DragEnter dest ->
            onDragEnter m dest

        DragEnd ->
            -- During HTML5 drag, KeyboardEvent won't fire (modifier key situations are accessible via DragEvent though).
            -- So we always turn off swap mode at dragend
            persist ( { m | viewState = { viewState | columnSwappable = False, columnSwapMaybe = Nothing } }, Cmd.none )

        Load val ->
            -- Persist on Load, migrating to new encoding format if any
            persist <| reloadProducers <| ( loadSavedState m val, Cmd.none )

        ToggleConfig opened ->
            ( { m | viewState = { viewState | configOpen = opened } }, Cmd.none )

        ToggleColumnConfig cId bool ->
            updateColumn cId m <| \c -> { c | configOpen = bool, deleteGate = "" }

        AddColumnFilter cId filter ->
            persist <| updateColumn cId m <| \c -> { c | filters = Array.push filter c.filters, offset = Nothing, items = [] }

        SetColumnFilter cId index filter ->
            persist <| updateColumn cId m <| \c -> { c | filters = Array.set index filter c.filters, offset = Nothing, items = [] }

        DelColumnFilter cId index ->
            persist <| updateColumn cId m <| \c -> { c | filters = Array.removeAt index c.filters, offset = Nothing, items = [] }

        ColumnDeleteGateInput cId text ->
            updateColumn cId m <| \c -> { c | deleteGate = text }

        ProducerCtrl pctrl ->
            persist <| applyProducerYield m <| Producer.update pctrl m.producerRegistry

        ScanBroker _ ->
            persist <| scanBroker <| updateProducerFetchStatuses m

        NoOp ->
            ( m, Cmd.none )


addColumn : Model -> Model
addColumn m =
    let
        ( newId, newIdGen ) =
            UniqueId.gen "column" m.idGen
    in
    { m | columnStore = ColumnStore.add (Column.new newId) m.columnStore, idGen = newIdGen }


onDragEnter : Model -> Int -> ( Model, Cmd Msg )
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
            ( { m | columnStore = ColumnStore.applyOrder newOrder m.columnStore }, Cmd.none )

        Nothing ->
            ( m, Cmd.none )


updateColumn : String -> Model -> (Column -> Column) -> ( Model, Cmd Msg )
updateColumn cId m updater =
    ( { m | columnStore = ColumnStore.updateById cId updater m.columnStore }
        |> updateProducerFetchStatuses
    , Cmd.none
    )


updateProducerFetchStatuses : Model -> Model
updateProducerFetchStatuses m =
    -- This function should "fix" corrupted Producer statuses.
    { m
        | producerRegistry =
            m.producerRegistry
                |> Producer.discordSetChannelFetchStatus (ColumnStore.discordChannelIds m.columnStore)
    }


scanBroker : Model -> ( Model, Cmd Msg )
scanBroker m =
    ( { m | columnStore = ColumnStore.consumeBroker brokerScanChunkAmount m.itemBroker m.columnStore }
    , scheduleNextScan
    )


brokerScanChunkAmount : Int
brokerScanChunkAmount =
    500



-- STATE PERSISTENCE


{-| Persists Elm application state to IndexedDB via port.

Do NOT call this function on every update, instead use it only when subject-to-persist data is updated,
in order to minimize IndexedDB access.

Even queueing/throttling can be introduced later, for when IndexedDB access become too frequent
(with risks of potential data loss, obviously.)

-}
persist : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
persist ( model, cmd ) =
    ( model
    , if model.env.indexedDBAvailable then
        Cmd.batch [ cmd, Ports.sendToJs (encodeModel model) ]

      else
        cmd
    )


encodeModel : Model -> E.Value
encodeModel m =
    E.object
        [ ( "columnStore", ColumnStore.encode m.columnStore )
        , ( "itemBroker", Broker.encode Item.encode m.itemBroker )
        , ( "producerRegistry", Producer.encodeRegistry m.producerRegistry )
        , ( "idGen", UniqueId.encodeGenerator m.idGen )
        ]


{-| Decodes JS value from IndexedDB and populate Model.

This function is called upon Load Msg, not in init with flags,
since all IndexedDB APIs are asynchronous.

-}
loadSavedState : Model -> D.Value -> Model
loadSavedState model value =
    case D.decodeValue (savedStateDecoder model.idGen) value of
        Ok savedState ->
            { model
                | columnStore = savedState.columnStore
                , itemBroker = savedState.itemBroker
                , producerRegistry = savedState.producerRegistry
                , idGen = savedState.idGen
            }

        Err err ->
            let
                m =
                    welcomeModel model.env model.navKey
            in
            { m | log = Logger.rec m.log (Entry "Error - loadSavedState" [ D.errorToString err ]) }


type alias SavedState =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueId.Generator
    }


savedStateDecoder : UniqueId.Generator -> Decoder SavedState
savedStateDecoder idGen =
    -- Write new decoder and migration logic when you change SavedState structure
    D.oneOf
        [ v3StateDecoder
        , v2StateDecoder
        , v1StateDecoder idGen
        ]


v3StateDecoder : Decoder SavedState
v3StateDecoder =
    D.map4 SavedState
        (D.field "columnStore" ColumnStore.decoder)
        (D.maybeField "itemBroker" (Broker.decoder Item.decoder) |> D.map (Maybe.withDefault ItemBroker.init))
        (D.field "producerRegistry" Producer.registryDecoder)
        (D.field "idGen" UniqueId.generatorDecoder)


v2StateDecoder : Decoder SavedState
v2StateDecoder =
    D.map convertFromV2State <|
        D.map2 Tuple.pair
            (D.field "columnStore" ColumnStore.decoder)
            (D.field "idGen" UniqueId.generatorDecoder)


convertFromV2State : ( ColumnStore, UniqueId.Generator ) -> SavedState
convertFromV2State ( columnStore, idGen ) =
    SavedState columnStore ItemBroker.init Producer.initRegistry idGen


v1StateDecoder : UniqueId.Generator -> Decoder SavedState
v1StateDecoder idGen =
    D.field "columns" (D.list Column.decoder)
        |> D.andThen (convertFromV1State idGen)
        |> D.map convertFromV2State


convertFromV1State : UniqueId.Generator -> List Column -> Decoder ( ColumnStore, UniqueId.Generator )
convertFromV1State idGen columns =
    case columns of
        (_ :: _) as nonEmptyColumns ->
            let
                applyId decoded ( accColumnStore, accIdGen ) =
                    UniqueId.genAndMap "column" accIdGen <|
                        \newId ->
                            ColumnStore.add { decoded | id = newId } accColumnStore
            in
            D.succeed <| List.foldr applyId ( ColumnStore.init, idGen ) nonEmptyColumns

        [] ->
            D.fail "No saved columns. Go to fallback."



-- PRODUCER


{-| Reload Producers on saved state load.

After the initial engage, subsequent engage/disengage should be done
on demand generated as Producer Replys.

-}
reloadProducers : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reloadProducers ( m, cmd ) =
    let
        ( newRegistry, reloadCmd ) =
            Producer.reloadAll m.producerRegistry
    in
    ( { m | producerRegistry = newRegistry }, Cmd.batch [ Cmd.map ProducerCtrl reloadCmd, cmd ] )


applyProducerYield : Model -> Producer.GrossYield -> ( Model, Cmd Msg )
applyProducerYield model gy =
    case gy.items of
        [] ->
            ( { model | producerRegistry = gy.producerRegistry }, Cmd.map ProducerCtrl gy.cmd )

        nonEmptyYields ->
            ( { model
                | itemBroker = ItemBroker.bulkAppend nonEmptyYields model.itemBroker
                , producerRegistry = gy.producerRegistry
              }
            , Cmd.map ProducerCtrl gy.cmd
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ onResize Resize
        , Ports.loadFromJs Load
        , toggleColumnSwap m.viewState.columnSwappable
        ]


toggleColumnSwap : Bool -> Sub Msg
toggleColumnSwap swappable =
    Sub.batch
        [ if not swappable then
            Browser.Events.onKeyDown (D.field "altKey" D.bool |> D.map ToggleColumnSwappable)

          else
            Browser.Events.onKeyUp (D.field "altKey" D.bool |> D.map ToggleColumnSwappable)
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



-- MAIN


main : Program Env Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = log update
        , subscriptions = sub
        , onUrlRequest = LinkClicked
        , onUrlChange = \_ -> NoOp
        }


log : (Msg -> Model -> ( Model, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
log u msg m =
    u msg { m | log = Logger.rec m.log (msgToLogEntry msg) }


msgToLogEntry : Msg -> Entry
msgToLogEntry msg =
    case msg of
        NoOp ->
            Entry "NoOp" []

        Resize x y ->
            Entry "Resize" [ fromInt x, fromInt y ]

        GetViewport _ ->
            Entry "GetViewport" [ "<viewport>" ]

        LinkClicked (Internal url) ->
            Entry "LinkClicked - Internal" [ Url.toString url ]

        LinkClicked (External str) ->
            Entry "LinkClicked - External" [ str ]

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

        Load _ ->
            Entry "Load" [ "<savedState>" ]

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


producerMsgToEntry : Producer.Msg -> Entry
producerMsgToEntry pMsg =
    case pMsg of
        Producer.DiscordMsg msgDiscord ->
            case msgDiscord of
                Discord.TokenInput input ->
                    Entry "Discord - TokenInput" [ input ]

                Discord.CommitToken ->
                    Entry "Discord - CommitToken" []

                Discord.Identify user ->
                    Entry "Discord - Identify" [ E.encode 2 (Discord.encodeUser user) ]

                Discord.Hydrate _ _ ->
                    Entry "Discord - Hydrate" [ "<Hydrate>" ]

                Discord.Rehydrate ->
                    Entry "Discord - Rehydrate" []

                Discord.Fetch posix ->
                    Entry "Discord - Fetch" [ Iso8601.fromTime posix ]

                Discord.Fetched (Discord.FetchOk cId ms posix) ->
                    Entry "Discord - FetchOk" [ cId, Iso8601.fromTime posix, E.encode 2 (E.list Discord.encodeMessage ms) ]

                Discord.Fetched (Discord.FetchErr cId e) ->
                    Entry "Discord - FetchErr" [ cId, HttpExtra.errorToString e ]

                Discord.APIError e ->
                    Entry "Discord - APIError" [ HttpExtra.errorToString e ]
