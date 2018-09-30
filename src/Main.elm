module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (Visibility(..), onResize)
import Browser.Navigation as Nav exposing (Key)
import Data.Array as Array
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Core as Core exposing (ColumnSwap, Env, Model, Msg(..), welcomeModel)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.UniqueId as UniqueId
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Ports
import Task
import Url
import View



-- INIT


init : Env -> url -> Key -> ( Model, Cmd Msg )
init env _ navKey =
    Core.init env navKey
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, adjustMaxHeight ])


adjustMaxHeight : Cmd Msg
adjustMaxHeight =
    Task.perform GetViewport getViewport



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ uiState, env } as m) =
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

        AddColumn ->
            persist ( addColumn m, Cmd.none )

        DelColumn index ->
            persist ( { m | columnStore = ColumnStore.removeAt index m.columnStore }, Cmd.none )

        ToggleColumnSwappable bool ->
            ( { m | uiState = { uiState | columnSwappable = bool } }, Cmd.none )

        DragStart originalIndex grabbedId ->
            let
                columnSwap =
                    ColumnSwap grabbedId originalIndex m.columnStore.order
            in
            ( { m | uiState = { uiState | columnSwapMaybe = Just columnSwap } }, Cmd.none )

        DragEnter dest ->
            onDragEnter m dest

        DragEnd ->
            -- During HTML5 drag, KeyboardEvent won't fire (modifier key situations are accessible via DragEvent though).
            -- So we always turn off swap mode at dragend
            persist ( { m | uiState = { uiState | columnSwappable = False, columnSwapMaybe = Nothing } }, Cmd.none )

        Load val ->
            ( loadSavedState m val, Cmd.none )
                |> engageProducers
                |> persist

        WSReceive val ->
            Producer.receive ProducerCtrl m.producerRegistry m.wsState val
                |> applyProducerReceipt m
                |> persist

        ToggleConfig opened ->
            ( { m | uiState = { uiState | configOpen = opened } }, Cmd.none )

        ProducerCtrl pctrl ->
            Producer.update ProducerCtrl pctrl m.wsState m.producerRegistry
                |> applyProducerReceipt m
                |> persist

        NoOp ->
            ( m, Cmd.none )


addColumn : Model -> Model
addColumn m =
    let
        ( newId, newIdGen ) =
            UniqueId.gen "column" m.idGen
    in
    { m | columnStore = ColumnStore.add (Column.welcome newId) m.columnStore, idGen = newIdGen }


onDragEnter : Model -> Int -> ( Model, Cmd Msg )
onDragEnter m dest =
    -- Ideally we should pass originalOrder Array along with messages
    -- so that this case clause can be eliminated. ("Make impossible states unrepresentable.")
    -- However currently there is a bug that prevents --debug compilation
    -- when Arrays are passed in messages. See https://github.com/elm/compiler/issues/1753
    case m.uiState.columnSwapMaybe of
        Just swap ->
            let
                newOrder =
                    Array.moveFromTo swap.originalIndex dest swap.originalOrder
            in
            ( { m | columnStore = ColumnStore.applyOrder newOrder m.columnStore }, Cmd.none )

        Nothing ->
            ( m, Cmd.none )



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
                , producerRegistry = savedState.producerRegistry
                , idGen = savedState.idGen
            }

        _ ->
            welcomeModel model.env model.navKey


type alias SavedState =
    { columnStore : ColumnStore
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
    D.map3 SavedState
        (D.field "columnStore" ColumnStore.decoder)
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
    SavedState columnStore Producer.initRegistry idGen


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


{-| Engage Producers on saved state load.

After the initial engage, subsequent engage/disengage should be done
on demand generated as Producer Replys.

-}
engageProducers : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
engageProducers ( m, cmd ) =
    let
        ( wsState, engageCmd ) =
            Producer.engageAll m.wsState m.producerRegistry
    in
    ( { m | wsState = wsState }, Cmd.batch [ engageCmd, cmd ] )


applyProducerReceipt : Model -> Producer.Receipt Msg -> ( Model, Cmd Msg )
applyProducerReceipt model { producerRegistry, wsState, cmd, yields } =
    case yields of
        [] ->
            ( { model | producerRegistry = producerRegistry, wsState = wsState }, cmd )

        nonEmptyYields ->
            let
                ( newColumnStore, newIdGen ) =
                    -- XXX Just for debugging. They should go to data broker eventually.
                    ColumnStore.pushToFirstColumn model.idGen nonEmptyYields model.columnStore
            in
            ( { model
                | columnStore = newColumnStore
                , producerRegistry = producerRegistry
                , idGen = newIdGen
                , wsState = wsState
              }
            , cmd
            )



-- SUB


sub : Model -> Sub Msg
sub m =
    Sub.batch
        [ onResize Resize
        , Ports.loadFromJs Load
        , Ports.webSocketClientSub WSReceive
        , toggleColumnSwap m.uiState.columnSwappable
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
        , update = update
        , subscriptions = sub
        , onUrlRequest = LinkClicked
        , onUrlChange = \_ -> NoOp
        }
