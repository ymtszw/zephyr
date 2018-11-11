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
            ( { m | idGen = idGen, columnStore = ColumnStore.add c m.columnStore }, Cmd.none, saveColumnStore changeSet )

        DelColumn index ->
            ( { m | columnStore = ColumnStore.removeAt index m.columnStore }, Cmd.none, saveColumnStore changeSet )

        ToggleColumnSwappable True ->
            pure { m | viewState = { viewState | columnSwappable = True } }

        ToggleColumnSwappable False ->
            -- DragEnd may get lost. Fix zombie drag here.
            pure { m | viewState = { viewState | columnSwappable = False, columnSwapMaybe = Nothing } }

        DragStart originalIndex colId ->
            pure { m | viewState = { viewState | columnSwapMaybe = Just (ColumnSwap colId originalIndex m.columnStore.order) } }

        DragEnter newOrder ->
            pure { m | columnStore = ColumnStore.applyOrder newOrder m.columnStore }

        DragEnd ->
            -- During HTML5 drag, KeyboardEvent won't fire (modifier key situations are accessible via DragEvent though).
            -- So we always turn off swap mode at dragend
            pure { m | viewState = { viewState | columnSwappable = False, columnSwapMaybe = Nothing } }

        LoadColumnStore ( cs, idGen ) ->
            noPersist ( { m | columnStore = cs, idGen = idGen }, IndexedDb.requestItemBroker )

        LoadItemBroker itemBroker ->
            noPersist ( { m | itemBroker = itemBroker }, IndexedDb.requestProducerRegistry )

        LoadProducerRegistry pr ->
            reloadProducers { m | producerRegistry = pr, worque = Worque.push Worque.BrokerScan m.worque }

        LoadOk ss ->
            -- TODO break them apart; save/load one gigantic state object is one of anti-pattern
            reloadProducers <|
                { m
                    | columnStore = ss.columnStore
                    , itemBroker = ss.itemBroker
                    , producerRegistry = ss.producerRegistry
                    , idGen = ss.idGen
                    , worque = Worque.push Worque.BrokerScan m.worque
                }

        LoadErr _ ->
            -- Hard failure of initial state load; start app normally as the last resport
            pure { m | worque = Worque.push Worque.BrokerScan m.worque }

        ToggleConfig opened ->
            pure { m | viewState = { viewState | configOpen = opened } }

        ColumnCtrl cId cMsg ->
            let
                ( cs, cmd, persist ) =
                    ColumnStore.updateById cId cMsg m.columnStore
            in
            ( { m | columnStore = cs }
            , Cmd.map (ColumnCtrl cId) cmd
            , if persist then
                saveColumnStore changeSet

              else
                changeSet
            )

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
onTick posix m =
    case Worque.pop m.worque of
        ( Just BrokerScan, newWorque ) ->
            let
                ( cs, persist ) =
                    ColumnStore.consumeBroker m.env.clientHeight m.itemBroker m.columnStore
            in
            ( { m | columnStore = cs, worque = Worque.push BrokerScan newWorque }
            , Cmd.none
            , if persist then
                saveColumnStore changeSet

              else
                changeSet
            )

        ( Just DiscordFetch, newWorque ) ->
            Producer.update (Producer.DiscordMsg (Discord.Fetch posix)) m.producerRegistry
                |> applyProducerYield { m | worque = newWorque }

        ( Nothing, newWorque ) ->
            pure { m | worque = newWorque }


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


{-| Restart producers on savedState reload.

Always persist producerRegistry in order to apply new encoding format, if any.

-}
reloadProducers : Model -> ( Model, Cmd Msg, ChangeSet )
reloadProducers ({ viewState } as m) =
    let
        reloaded =
            Producer.reloadAll m.producerRegistry
    in
    ( { m
        | producerRegistry = reloaded.producerRegistry
        , worque = Worque.pushAll reloaded.works m.worque
        , viewState = { viewState | filterAtomMaterial = FilterAtomMaterial.update reloaded.famInstructions viewState.filterAtomMaterial }
      }
    , Cmd.map ProducerCtrl reloaded.cmd
    , saveProducerRegistry changeSet
    )


applyProducerYield : Model -> Producer.Yield -> ( Model, Cmd Msg, ChangeSet )
applyProducerYield ({ viewState } as m) y =
    case y.items of
        [] ->
            ( { m
                | producerRegistry = y.producerRegistry
                , worque = y.postProcess.work |> Maybe.map (\w -> Worque.push w m.worque) |> Maybe.withDefault m.worque
                , viewState =
                    { viewState
                        | filterAtomMaterial =
                            FilterAtomMaterial.update [ y.postProcess.famInstruction ] viewState.filterAtomMaterial
                    }
              }
            , Cmd.map ProducerCtrl y.cmd
            , if y.postProcess.persist then
                saveProducerRegistry changeSet

              else
                changeSet
            )

        nonEmptyYields ->
            ( { m
                | itemBroker = ItemBroker.bulkAppend nonEmptyYields m.itemBroker
                , producerRegistry = y.producerRegistry
                , worque = y.postProcess.work |> Maybe.map (\w -> Worque.push w m.worque) |> Maybe.withDefault m.worque
                , viewState =
                    { viewState
                        | filterAtomMaterial =
                            FilterAtomMaterial.update [ y.postProcess.famInstruction ] viewState.filterAtomMaterial
                    }
              }
            , Cmd.map ProducerCtrl y.cmd
            , if y.postProcess.persist then
                changeSet |> saveProducerRegistry |> saveItemBroker

              else
                saveItemBroker changeSet
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
        , toggleColumnSwap m.viewState.columnSwappable
        ]


globalTimerIntervalMillis : Float
globalTimerIntervalMillis =
    -- 6 Hz
    166.7


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
