module Data.Core exposing (ColumnSwap, Env, Model, Msg(..), init, welcomeModel)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport)
import Browser.Navigation exposing (Key)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item)
import Data.Producer as Producer exposing (Producer, Storage)
import Data.UniqueId as UniqueId exposing (Generator)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Websocket



-- MODEL


type alias Model =
    { columnStore : ColumnStore
    , columnSwappable : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    , producers : List Producer
    , idGen : Generator
    , navKey : Key
    , wsState : Websocket.State Msg Storage Item
    , env : Env
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , clientHeight : Int
    }


type alias ColumnSwap =
    { grabbedId : String
    , originalIndex : Int
    , originalOrder : Array String
    }


init : Env -> Key -> ( Model, Cmd Msg )
init env navKey =
    initModel env navKey
        |> installProducers


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        Model
            ColumnStore.init
            False
            Nothing
            []
            UniqueId.init
            navKey
            (Websocket.init [])
            env

    else
        welcomeModel env navKey


installProducers : Model -> ( Model, Cmd Msg )
installProducers m =
    let
        { idGen, wsState, cmd } =
            Producer.installAll m.idGen m.wsState m.producers
    in
    ( { m | idGen = idGen, wsState = wsState }, cmd )


welcomeModel : Env -> Key -> Model
welcomeModel env navKey =
    let
        ( columnStore, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <|
                \newId -> ColumnStore.add (Column.welcome newId) ColumnStore.init
    in
    Model
        columnStore
        False
        Nothing
        []
        idGen
        navKey
        (Websocket.init [])
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
