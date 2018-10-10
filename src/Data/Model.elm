module Data.Model exposing (ColumnSwap, Env, Model, ViewState, init, welcomeModel)

import Array exposing (Array)
import Browser.Navigation exposing (Key)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Msg exposing (Msg)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.UniqueId as UniqueId
import View.Select


type alias Model =
    { columnStore : ColumnStore
    , producerRegistry : ProducerRegistry
    , idGen : UniqueId.Generator
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


type alias ViewState =
    { configOpen : Bool
    , columnSwappable : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    , selectState : View.Select.State
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
        , viewState = defaultViewState
        , env = env
        }

    else
        welcomeModel env navKey


defaultViewState : ViewState
defaultViewState =
    { configOpen = False
    , columnSwappable = False
    , columnSwapMaybe = Nothing
    , selectState = View.Select.init
    }


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
    , viewState = defaultViewState
    , env = env
    }
