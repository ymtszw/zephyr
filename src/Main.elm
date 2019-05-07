module Main exposing (main)

{-| Entry point of Zephyr main app.

The application's Model starts with almost-empty initial model.

  - If IndexedDB is unavailable, it immediately transits to Model.welcome.
  - If IndexedDB is available, it subscribes to state load:
      - State loading is initiated from JavaScript codes in static/index.html
      - It loads saved states in the order of ColumnStore => ItemBroker => ProducerRegistry
      - If the saved states are corrupted and somehow cannot be decoded,
        it defaults to appropriate preset values.
      - If a decoding attempt failed without any clue of "phase",
        it bails out the sequence as the last resport.

-}

import Broker
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.ItemBroker as ItemBroker
import Data.Model as Model exposing (Env, Model)
import Data.Msg exposing (..)
import Data.Pref as Pref exposing (Pref)
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Data.ProducerRegistry as ProducerRegistry exposing (ProducerRegistry)
import Id
import IndexedDb exposing (..)
import Task exposing (Task)
import Time exposing (Posix)
import TimeZone
import Url
import View.Atoms.Input.Select
import View.Organisms.Modeless as Modeless
import View.Pages.Main
import View.Stylesheet
import View.Templates.Main exposing (columnAreaParentId, columnWidth)
import Worque exposing (..)



-- MAIN


main : Program Env Model Msg
main =
    Browser.application
        { init = init
        , update = \msg m -> update msg m |> IndexedDb.postUpdate
        , subscriptions = sub
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = \_ -> NoOp
        }



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    ( Model.init env navKey, Cmd.batch [ adjustMaxHeight, getTimeZone ] )


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
update msg ({ env, pref, viewState } as m) =
    case msg of
        Resize _ _ ->
            -- Not using onResize event values directly; they are basically innerWidth/Height which include scrollbars
            noPersist ( m, adjustMaxHeight )

        GetViewport { viewport } ->
            -- On the other hand, getViewport is using clientHeight, which does not include scrollbars.
            -- Scrolls are resized on BrokerScan
            pure
                { m
                    | env = { env | clientHeight = round viewport.height, clientWidth = round viewport.width }
                    , pref = { pref | evictThreshold = Pref.adjustEvictThreashold (round viewport.width) }
                }

        GetTimeZone ( _, zone ) ->
            pure { m | viewState = { viewState | timezone = zone } }

        VisibilityChanged visible ->
            pure { m | viewState = { viewState | visible = visible } }

        LinkClicked (Browser.Internal url) ->
            noPersist ( m, Nav.pushUrl m.navKey (Url.toString url) )

        LinkClicked (Browser.External url) ->
            noPersist ( m, Nav.load url )

        SelectCtrl sMsg ->
            let
                ( ss, cmd ) =
                    View.Atoms.Input.Select.update SelectCtrl sMsg viewState.selectState
            in
            noPersist ( { m | viewState = { viewState | selectState = ss } }, cmd )

        LoadColumnStore ( cs, initCmd ) ->
            ( { m | columnStore = cs }
            , Cmd.batch [ IndexedDb.requestItemBroker, IndexedDb.requestPref, initCmd ]
            , saveColumnStore changeSet
            )

        LoadItemBroker itemBroker ->
            if Broker.capacity itemBroker == Broker.capacity m.itemBroker then
                ( { m | itemBroker = itemBroker }, IndexedDb.requestProducerRegistry, saveItemBroker changeSet )

            else
                -- Broker reset and migration; reset columns' Offsets, discarding old itemBroker
                ( { m | columnStore = ColumnStore.map (Column.setOffset Nothing) m.columnStore }
                , IndexedDb.requestProducerRegistry
                , changeSet |> saveItemBroker |> saveColumnStore
                )

        LoadProducerRegistry pr ->
            reloadProducers <|
                { m | producerRegistry = pr, worque = Worque.pushAll [ DropOldState, initScan m.columnStore ] m.worque }

        LoadPref loaded ->
            -- Pref decoding always succeeds, and it is not a part of critical state loading chain.
            ( { m | pref = loaded }, Cmd.none, savePref changeSet )

        LoadErr _ ->
            pure <|
                if ColumnStore.size m.columnStore == 0 then
                    -- Presumed first visit
                    Model.addWelcomeColumn { m | worque = Worque.push (initScan m.columnStore) m.worque }

                else
                    -- Hard failure of initial state load; start app normally as the last resport
                    { m | worque = Worque.push (initScan m.columnStore) m.worque }

        ToggleConfig opened ->
            pure { m | viewState = { viewState | configOpen = opened } }

        ColumnCtrl csMsg ->
            applyColumnUpdate m <| ColumnStore.update (columnLimit m.pref) csMsg m.columnStore

        ProducerCtrl pctrl ->
            applyProducerYield m <| ProducerRegistry.update pctrl m.producerRegistry

        Tick posix ->
            onTick posix m

        DomOp (Ok ()) ->
            pure m

        DomOp (Err _) ->
            pure m

        PrefCtrl pMsg ->
            case Pref.update pMsg pref of
                ( newPref, True ) ->
                    ( { m | pref = newPref }, Cmd.none, savePref changeSet )

                ( newPref, False ) ->
                    pure { m | pref = newPref }

        ModelessTouch mId ->
            pure { m | viewState = { viewState | modeless = Modeless.touch mId viewState.modeless } }

        ModelessMove mId x y ->
            pure { m | viewState = { viewState | modeless = Modeless.move ( mId, x, y ) viewState.modeless } }

        ModelessRemove mId ->
            pure { m | viewState = { viewState | modeless = Modeless.remove mId viewState.modeless } }

        NoOp ->
            pure m


pure : Model -> ( Model, Cmd Msg, ChangeSet )
pure m =
    ( m, Cmd.none, changeSet )


columnLimit : Pref -> Maybe Int
columnLimit pref =
    if pref.zephyrMode then
        Just pref.evictThreshold

    else
        Nothing


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
                scanOpts =
                    { broker = m.itemBroker
                    , maxCount = maxScanCount // ColumnStore.size m.columnStore -- Rough throttling. XXX Unnecessary?? Or, seek better ways to "resuming"
                    , clientHeight = m.env.clientHeight
                    , catchUp = False
                    }

                scheduleNextScan ( mNew, cmd, cs ) =
                    ( { mNew | worque = Worque.push (initScan mNew.columnStore) mNew.worque }, cmd, cs )
            in
            m.columnStore
                |> ColumnStore.update (columnLimit m.pref) (ColumnStore.ConsumeBroker scanOpts)
                |> applyColumnUpdate m
                |> scheduleNextScan

        Just (BrokerScan n) ->
            pure { m | worque = Worque.push (BrokerScan (n - 1)) m.worque }

        Just DiscordFetch ->
            ProducerRegistry.update (ProducerRegistry.DiscordMsg (Discord.Fetch posix)) m.producerRegistry
                |> applyProducerYield m

        Just SlackFetch ->
            ProducerRegistry.update (ProducerRegistry.SlackMsg (Slack.Fetch posix)) m.producerRegistry
                |> applyProducerYield m

        Just DropOldState ->
            -- Finalize migration; may remove if migration propagated
            noPersist ( m, IndexedDb.dropOldState )

        Just (BrokerCatchUp cId) ->
            let
                scanOpts =
                    { broker = m.itemBroker
                    , maxCount = maxScanCount
                    , clientHeight = m.env.clientHeight
                    , catchUp = True
                    }
            in
            m.columnStore
                |> ColumnStore.update (columnLimit m.pref) (ColumnStore.ById (Id.from cId) (Column.ScanBroker scanOpts))
                |> applyColumnUpdate m

        Nothing ->
            pure m


maxScanCount : Int
maxScanCount =
    500


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


applyColumnUpdate : Model -> ( ColumnStore, ColumnStore.PostProcess ) -> ( Model, Cmd Msg, ChangeSet )
applyColumnUpdate m ( columnStore, pp ) =
    let
        m_ =
            case pp.catchUpId of
                Just id ->
                    { m
                        | columnStore = columnStore
                        , worque = Worque.push (BrokerCatchUp (Id.to id)) m.worque
                    }

                Nothing ->
                    { m | columnStore = columnStore }

        finalize ( n, cmd, changeSet_ ) =
            ( n
            , Cmd.batch [ Cmd.map ColumnCtrl pp.cmd, cmd ]
            , if pp.persist then
                saveColumnStore changeSet_

              else
                changeSet_
            )
    in
    finalize <|
        case pp.producerMsg of
            Just pMsg ->
                applyProducerYield m_ <| ProducerRegistry.update pMsg m_.producerRegistry

            Nothing ->
                ( m_, Cmd.none, changeSet )


{-| Restart producers on application state reload.

Always persist producerRegistry in order to apply new encoding format, if any.

-}
reloadProducers : Model -> ( Model, Cmd Msg, ChangeSet )
reloadProducers m =
    let
        ( producerRegistry, gr ) =
            ProducerRegistry.reloadAll m.producerRegistry

        ( columnStore, _ ) =
            ColumnStore.updateFAM gr.famInstructions m.columnStore
    in
    ( { m
        | producerRegistry = producerRegistry
        , columnStore = columnStore
        , worque = Worque.pushAll gr.works m.worque
      }
    , Cmd.map ProducerCtrl gr.cmd
    , changeSet |> saveProducerRegistry |> saveColumnStore
    )


applyProducerYield : Model -> ( ProducerRegistry, ProducerRegistry.Yield ) -> ( Model, Cmd Msg, ChangeSet )
applyProducerYield m_ ( producerRegistry, y ) =
    let
        ( columnStore, persistColumnStore ) =
            ColumnStore.updateFAM [ y.famInstruction ] m_.columnStore

        m =
            { m_
                | producerRegistry = producerRegistry
                , columnStore = columnStore
                , worque =
                    case y.work of
                        Just w ->
                            Worque.push w m_.worque

                        Nothing ->
                            m_.worque
            }

        changeSetBase =
            case ( y.persist, persistColumnStore ) of
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
            IndexedDb.load m.env

          else
            Sub.none
        , Time.every tickIntervalMillis Tick
        , Browser.Events.onVisibilityChange <|
            \visibility ->
                VisibilityChanged <|
                    case visibility of
                        Browser.Events.Visible ->
                            True

                        Browser.Events.Hidden ->
                            False
        , View.Atoms.Input.Select.sub SelectCtrl m.viewState.selectState
        , Modeless.sub ModelessMove m.viewState.modeless
        ]


tickIntervalMillis : Float
tickIntervalMillis =
    -- 10 Hz
    100.0



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Zephyr"
    , body = View.Stylesheet.render :: View.Pages.Main.render m
    }
