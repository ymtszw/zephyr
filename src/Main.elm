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

        AddSimpleColumn fa ->
            UniqueId.genAndMap columnIdPrefix m.idGen (Column.simple fa)
                |> (\( c, idGen ) -> ( { m | idGen = idGen, columnStore = ColumnStore.add c m.columnStore }, Cmd.none, True ))

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

        ColumnCtrl cId cMsg ->
            map (\cs -> { m | columnStore = cs }) (ColumnCtrl cId) <| ColumnStore.updateById cId cMsg m.columnStore

        ProducerCtrl pctrl ->
            applyProducerYield m <| Producer.update pctrl m.producerRegistry

        Tick posix ->
            onTick posix m

        RevealColumn index ->
            ( m, revealColumn index, False )

        DomOp (Ok ()) ->
            pure m

        DomOp (Err e) ->
            pure m

        NoOp ->
            pure m


addColumn : Model -> Model
addColumn m =
    let
        ( newColumn, newIdGen ) =
            m.idGen
                |> UniqueId.gen columnIdPrefix
                |> UniqueId.andThen (\( cId, idGen ) -> Column.new idGen cId)
    in
    { m | columnStore = ColumnStore.add newColumn m.columnStore, idGen = newIdGen }


columnIdPrefix : String
columnIdPrefix =
    "column"


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


onTick : Posix -> Model -> ( Model, Cmd Msg, Bool )
onTick posix mOld =
    case Worque.pop mOld.worque of
        ( Just BrokerScan, newWorque ) ->
            scanBroker { mOld | worque = newWorque }

        ( Just DiscordFetch, newWorque ) ->
            Producer.update (Producer.DiscordMsg (Discord.Fetch posix)) mOld.producerRegistry
                |> applyProducerYield { mOld | worque = newWorque }

        ( Nothing, newWorque ) ->
            pure { mOld | worque = newWorque }


scanBroker : Model -> ( Model, Cmd Msg, Bool )
scanBroker m =
    let
        ( newColumnStore, shouldPersist ) =
            ColumnStore.consumeBroker m.itemBroker m.columnStore
    in
    ( { m | columnStore = newColumnStore, worque = Worque.push BrokerScan m.worque }, Cmd.none, shouldPersist )


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

Always persist state in order to apply new encoding format, if any.

-}
reloadProducers : Model -> ( Model, Cmd Msg, Bool )
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
    , True
    )


applyProducerYield : Model -> Producer.Yield -> ( Model, Cmd Msg, Bool )
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
            , y.postProcess.persist
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
            , y.postProcess.persist
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ Browser.Events.onResize Resize
        , IndexedDb.load m.idGen
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
