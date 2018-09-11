module Data.Core exposing (ColumnSwap, Env, Model, Msg(..), init, welcomeModel)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport)
import Browser.Navigation exposing (Key)
import Data.Column as Column exposing (Column)
import Data.Item exposing (Item)
import Data.Producer as Producer exposing (Producer, Storage)
import Data.UniqueId as UniqueId exposing (Generator)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Websocket



-- MODEL


type alias Model =
    { columns : Array Column
    , columnSwap : ColumnSwap
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
    { handleMaybe : Maybe Int
    , hoverMaybe : Maybe Int
    , swapping : Bool
    }


init : Env -> Key -> ( Model, Cmd Msg )
init env navKey =
    initModel env navKey
        |> installProducers


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        { columns = Array.fromList []
        , columnSwap = ColumnSwap Nothing Nothing False
        , producers = []
        , idGen = UniqueId.init
        , navKey = navKey
        , wsState = Websocket.init []
        , env = env
        }

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
        ( columns, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <| \newId -> [ Column.welcome newId ]
    in
    { columns = Array.fromList columns
    , columnSwap = ColumnSwap Nothing Nothing False
    , producers = []
    , idGen = idGen
    , navKey = navKey
    , wsState = Websocket.init []
    , env = env
    }



-- MSG


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Viewport
    | LinkClicked UrlRequest
    | AddColumn
    | DelColumn Int
    | MakeDraggable Int
    | GoUndraggable Int
    | SwapStart Int
    | SwapEnd Int
    | DragHover Int
    | DragLeave Int
    | Drop Int Int
    | Load Value
    | WSReceive Value
