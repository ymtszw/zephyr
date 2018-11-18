module Main exposing (main)

{-| Entry point of Zephyr main app.

The application's Model starts with almost-empty initial model.

  - If IndexedDB is unavailable, it immediately transits to Model.welcome.
  - If IndexedDB is available, it subscribes to state load:
      - State loading is initiated from JavaScript codes in static/index.html
      - It loads saved states in the order of ColumnStore & UniqueIdGen => ItemBroker => ProducerRegistry
      - If the saved states are corrupted and somehow cannot be decoded,
        it defaults to appropriate preset values.
      - If a decoding attempt failed without any clue of "phase",
        it bails out the sequence as the last resport.

-}

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
import Data.UniqueIdGen as UniqueIdGen
import IndexedDb exposing (ChangeSet, changeSet, noPersist, saveColumnStore, saveItemBroker, saveProducerRegistry)
import Json.Decode as D
import Json.DecodeExtra as D
import Logger
import Task exposing (Task)
import Time exposing (Posix)
import TimeZone
import Url
import View
import View.Parts exposing (columnAreaParentId, fixedColumnWidth)
import View.Select
import Worque exposing (Work(..))



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


log : (Msg -> Model -> ( Model, Cmd Msg, ChangeSet )) -> Msg -> Model -> ( Model, Cmd Msg, ChangeSet )
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
    ( if env.indexedDBAvailable then
        Model.init env navKey

      else
        Model.welcome env navKey
    , Cmd.batch
        [ adjustMaxHeight
        , getTimeZone
        ]
    )


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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ChangeSet )
update msg ({ viewState, env } as m) =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            noPersist ( m, adjustMaxHeight )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars.
            -- Scrolls are resized on BrokerScan
            pure { m | env = { env | clientHeight = round viewport.height } }

        GetTimeZone ( _, zone ) ->
            pure { m | viewState = { viewState | timezone = zone } }

        VisibilityChanged True ->
            pure { m | viewState = { viewState | visible = True } }

        VisibilityChanged False ->
            pure { m | viewState = { viewState | columnSwapMaybe = Nothing, visible = False } }

        LoggerCtrl lMsg ->
            Logger.update lMsg m.log |> Tuple.mapBoth (\l -> { m | log = l }) (Cmd.map LoggerCtrl) |> IndexedDb.noPersist

        LinkClicked (Browser.Internal url) ->
            noPersist ( m, Nav.pushUrl m.navKey (Url.toString url) )

        LinkClicked (Browser.External url) ->
            noPersist ( m, Nav.load url )

        SelectToggle sId True ->
            pure { m | viewState = { viewState | selectState = View.Select.open sId viewState.selectState } }

        SelectToggle _ False ->
            pure { m | viewState = { viewState | selectState = View.Select.close } }

        SelectPick actualMsg ->
            update actualMsg { m | viewState = { viewState | selectState = View.Select.close } }

        AddEmptyColumn ->
            UniqueIdGen.gen UniqueIdGen.columnPrefix m.idGen
                |> UniqueIdGen.andThen (\( cId, idGen ) -> Column.new env.clientHeight idGen cId)
                |> (\( c, idGen ) ->
                        -- If Filters are somehow set to the new Column, then persist.
                        pure { m | columnStore = ColumnStore.add c m.columnStore, idGen = idGen }
                   )

        AddSimpleColumn fa ->
            let
                ( c, idGen ) =
                    UniqueIdGen.genAndMap UniqueIdGen.columnPrefix m.idGen (Column.simple env.clientHeight fa)
            in
            ( { m
                | idGen = idGen
                , columnStore = ColumnStore.add c m.columnStore
                , worque = Worque.push (BrokerCatchUp c.id) m.worque
              }
            , Cmd.none
            , saveColumnStore changeSet
            )

        DelColumn index ->
            ( { m | columnStore = ColumnStore.removeAt index m.columnStore }, Cmd.none, saveColumnStore changeSet )

        DragStart originalIndex colId ->
            pure { m | viewState = { viewState | columnSwapMaybe = Just (ColumnSwap colId originalIndex m.columnStore.order) } }

        DragEnter newOrder ->
            pure { m | columnStore = ColumnStore.applyOrder newOrder m.columnStore }

        DragEnd ->
            -- During HTML5 drag, KeyboardEvent won't fire (modifier key situations are accessible via DragEvent though).
            -- So we always turn off swap mode at dragend
            pure { m | viewState = { viewState | columnSwapMaybe = Nothing } }

        LoadColumnStore ( cs, idGen ) ->
            ( { m | columnStore = cs, idGen = idGen }, IndexedDb.requestItemBroker, saveColumnStore changeSet )

        LoadItemBroker itemBroker ->
            if Broker.capacity itemBroker == Broker.capacity m.itemBroker then
                ( { m | itemBroker = itemBroker }, IndexedDb.requestProducerRegistry, saveItemBroker changeSet )

            else
                -- Broker reset and migration; reset columns' Offsets, discarding old itemBroker
                ( { m | columnStore = ColumnStore.map (\c -> { c | offset = Nothing }) m.columnStore }
                , IndexedDb.requestProducerRegistry
                , changeSet |> saveItemBroker |> saveColumnStore
                )

        LoadProducerRegistry pr ->
            reloadProducers <|
                { m | producerRegistry = pr, worque = Worque.pushAll [ DropOldState, initScan m.columnStore ] m.worque }

        LoadOk ss ->
            -- Old method; remove after migration
            reloadProducers <|
                { m
                    | columnStore = ss.columnStore
                    , itemBroker = ss.itemBroker
                    , producerRegistry = ss.producerRegistry
                    , idGen = ss.idGen
                    , worque = Worque.push (initScan ss.columnStore) m.worque
                }

        LoadErr _ ->
            -- Hard failure of initial state load; start app normally as the last resport
            pure { m | worque = Worque.push (initScan m.columnStore) m.worque }

        ToggleConfig opened ->
            pure { m | viewState = { viewState | configOpen = opened } }

        ColumnCtrl cId cMsg ->
            applyColumnUpdate m cId <| ColumnStore.updateById cId cMsg m.columnStore

        ProducerCtrl pctrl ->
            applyProducerYield m <| Producer.update pctrl m.producerRegistry

        Tick posix ->
            onTick posix m

        RevealColumn index ->
            noPersist ( m, revealColumn index )

        DomOp (Ok ()) ->
            pure m

        DomOp (Err e) ->
            pure m

        NoOp ->
            pure m


pure : Model -> ( Model, Cmd Msg, ChangeSet )
pure m =
    ( m, Cmd.none, changeSet )


onTick : Posix -> Model -> ( Model, Cmd Msg, ChangeSet )
onTick posix m_ =
    let
        ( workMaybe, m ) =
            Worque.pop m_.worque
                |> Tuple.mapSecond (\newWorque -> { m_ | worque = newWorque })
    in
    case workMaybe of
        Just (BrokerScan 0) ->
            let
                ( cs, persist ) =
                    ColumnStore.consumeBroker m.env.clientHeight m.itemBroker m.columnStore
            in
            ( { m | columnStore = cs, worque = Worque.push (initScan cs) m.worque }
            , Cmd.none
            , if persist then
                saveColumnStore changeSet

              else
                changeSet
            )

        Just (BrokerScan n) ->
            pure { m | worque = Worque.push (BrokerScan (n - 1)) m.worque }

        Just DiscordFetch ->
            Producer.update (Producer.DiscordMsg (Discord.Fetch posix)) m.producerRegistry
                |> applyProducerYield m

        Just DropOldState ->
            -- Finalize migration; may remove if migration propagated
            noPersist ( m, IndexedDb.dropOldState )

        Just (BrokerCatchUp cId) ->
            case ColumnStore.catchUpBroker m.itemBroker cId m.columnStore of
                ( cs, True ) ->
                    ( { m | columnStore = cs, worque = Worque.push (BrokerCatchUp cId) m.worque }
                    , Cmd.none
                    , saveColumnStore changeSet
                    )

                ( cs, False ) ->
                    ( { m | columnStore = cs }, Cmd.none, saveColumnStore changeSet )

        Nothing ->
            pure m


initScan : ColumnStore -> Work
initScan cs =
    let
        numColumns =
            ColumnStore.size cs
    in
    if numColumns == 0 then
        BrokerScan maxScanInterval

    else
        BrokerScan (maxScanInterval // numColumns)


maxScanInterval : Int
maxScanInterval =
    10


revealColumn : Int -> Cmd Msg
revealColumn index =
    Browser.Dom.getViewportOf columnAreaParentId
        |> Task.andThen (scrollToColumn index)
        |> Task.attempt DomOp


scrollToColumn : Int -> Browser.Dom.Viewport -> Task Browser.Dom.Error ()
scrollToColumn index parentVp =
    let
        cWidth =
            toFloat fixedColumnWidth

        targetX =
            cWidth * toFloat index
    in
    if targetX < parentVp.viewport.x then
        Browser.Dom.setViewportOf columnAreaParentId targetX 0

    else if targetX + cWidth < parentVp.viewport.x + parentVp.viewport.width then
        Task.succeed ()

    else
        Browser.Dom.setViewportOf columnAreaParentId (targetX + cWidth - parentVp.viewport.width) 0


applyColumnUpdate : Model -> String -> ( ColumnStore, Column.PostProcess ) -> ( Model, Cmd Msg, ChangeSet )
applyColumnUpdate m cId ( columnStore, pp ) =
    let
        worque =
            case pp.catchUpId of
                Just id ->
                    Worque.push (BrokerCatchUp id) m.worque

                Nothing ->
                    m.worque

        changeSet_ =
            if pp.persist then
                saveColumnStore changeSet

            else
                changeSet
    in
    ( { m | columnStore = columnStore, worque = worque }, Cmd.map (ColumnCtrl cId) pp.cmd, changeSet_ )


{-| Restart producers on savedState reload.

Always persist producerRegistry in order to apply new encoding format, if any.

-}
reloadProducers : Model -> ( Model, Cmd Msg, ChangeSet )
reloadProducers m =
    let
        reloaded =
            Producer.reloadAll m.producerRegistry

        ( newColumnStore, _ ) =
            ColumnStore.updateFAM reloaded.famInstructions m.columnStore
    in
    ( { m
        | producerRegistry = reloaded.producerRegistry
        , columnStore = newColumnStore
        , worque = Worque.pushAll reloaded.works m.worque
      }
    , Cmd.map ProducerCtrl reloaded.cmd
    , changeSet |> saveProducerRegistry |> saveColumnStore
    )


applyProducerYield : Model -> Producer.Yield -> ( Model, Cmd Msg, ChangeSet )
applyProducerYield m_ y =
    let
        ( newColumnStore, persistColumnStore ) =
            ColumnStore.updateFAM [ y.postProcess.famInstruction ] m_.columnStore

        m =
            { m_
                | producerRegistry = y.producerRegistry
                , columnStore = newColumnStore
                , worque =
                    case y.postProcess.work of
                        Just w ->
                            Worque.push w m_.worque

                        Nothing ->
                            m_.worque
            }

        changeSetBase =
            case ( y.postProcess.persist, persistColumnStore ) of
                ( True, True ) ->
                    changeSet |> saveProducerRegistry |> saveColumnStore

                ( True, False ) ->
                    changeSet |> saveProducerRegistry

                ( False, True ) ->
                    changeSet |> saveColumnStore

                ( False, False ) ->
                    changeSet
    in
    case y.items of
        [] ->
            ( m, Cmd.map ProducerCtrl y.cmd, changeSetBase )

        nonEmptyYields ->
            ( { m | itemBroker = ItemBroker.bulkAppend nonEmptyYields m.itemBroker }
            , Cmd.map ProducerCtrl y.cmd
            , saveItemBroker changeSetBase
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ Browser.Events.onResize Resize
        , if m.env.indexedDBAvailable then
            IndexedDb.load m.env.clientHeight m.idGen

          else
            Sub.none
        , Time.every globalTimerIntervalMillis Tick
        , Browser.Events.onVisibilityChange <|
            \visibility ->
                VisibilityChanged <|
                    case visibility of
                        Browser.Events.Visible ->
                            True

                        Browser.Events.Hidden ->
                            False
        ]


globalTimerIntervalMillis : Float
globalTimerIntervalMillis =
    -- 10 Hz
    100.0



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.body m
    }
