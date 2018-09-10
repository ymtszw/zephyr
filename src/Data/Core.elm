module Data.Core exposing (Env, Model, Msg(..), init, welcomeModel)

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


init : Env -> Key -> ( Model, Cmd Msg )
init env navKey =
    initModel env navKey
        |> installProducers


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        { columns = Array.fromList []
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
    | Load Value
    | WSReceive Value
