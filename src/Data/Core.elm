module Data.Core exposing (ColumnSwap, Env, Model, Msg(..), UIState, init, welcomeModel)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport)
import Browser.Navigation exposing (Key)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.UniqueId as UniqueId exposing (Generator)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Websocket



-- MODEL


type alias Model =
    { columnStore : ColumnStore
    , producerRegistry : ProducerRegistry
    , idGen : Generator
    , navKey : Key
    , wsState : Websocket.State Msg
    , uiState : UIState
    , env : Env
    }


type alias UIState =
    { configOpen : Bool
    , columnSwappable : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    }


type alias ColumnSwap =
    { grabbedId : String
    , originalIndex : Int
    , originalOrder : Array String
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , clientHeight : Int
    }


init : Env -> Key -> ( Model, Cmd Msg )
init env navKey =
    initModel env navKey
        |> engageProducers


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        Model
            ColumnStore.init
            Producer.initRegistry
            UniqueId.init
            navKey
            Websocket.init
            defaultUIState
            env

    else
        welcomeModel env navKey


defaultUIState : UIState
defaultUIState =
    UIState False False Nothing


engageProducers : Model -> ( Model, Cmd Msg )
engageProducers m =
    let
        ( wsState, cmd ) =
            Producer.engageAll m.wsState m.producerRegistry
    in
    ( { m | wsState = wsState }, cmd )


welcomeModel : Env -> Key -> Model
welcomeModel env navKey =
    let
        ( columnStore, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <|
                \newId -> ColumnStore.add (Column.welcome newId) ColumnStore.init
    in
    Model
        columnStore
        Producer.initRegistry
        idGen
        navKey
        Websocket.init
        defaultUIState
        env



-- MSG


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Viewport
    | LinkClicked UrlRequest
    | AddColumn
    | DelColumn Int
    | ToggleColumnSwappable Bool
    | DragStart Int String
    | DragEnter Int
    | DragEnd
    | Load Value
    | WSReceive Value
    | ToggleConfig Bool
