module Data.Core exposing (ColumnSwap, Env, Model, Msg(..), ViewState, init, welcomeModel)

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



-- MODEL


type alias Model =
    { columnStore : ColumnStore
    , producerRegistry : ProducerRegistry
    , idGen : Generator
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


type alias ViewState =
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
    ( initModel env navKey, Cmd.none )


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        { columnStore = ColumnStore.init
        , producerRegistry = Producer.initRegistry
        , idGen = UniqueId.init
        , navKey = navKey
        , viewState = defaultUIState
        , env = env
        }

    else
        welcomeModel env navKey


defaultUIState : ViewState
defaultUIState =
    ViewState False False Nothing


welcomeModel : Env -> Key -> Model
welcomeModel env navKey =
    let
        ( columnStore, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <|
                \newId -> ColumnStore.add (Column.welcome newId) ColumnStore.init
    in
    { columnStore = columnStore
    , producerRegistry = Producer.initRegistry
    , idGen = idGen
    , navKey = navKey
    , viewState = defaultUIState
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
    | ToggleColumnSwappable Bool
    | DragStart Int String
    | DragEnter Int
    | DragEnd
    | Load Value
    | ToggleConfig Bool
    | ToggleColumnConfig String Bool
    | ProducerCtrl Producer.Msg
