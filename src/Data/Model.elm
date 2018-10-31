module Data.Model exposing
    ( Model, ViewState, Env, ColumnSwap
    , init, welcomeModel, encodeForPersistence
    )

{-| Model of the app.


## Types

@docs Model, ViewState, Env, ColumnSwap


## APIs

@docs init, welcomeModel, encodeForPersistence

-}

import Array exposing (Array)
import Broker exposing (Broker)
import Browser.Navigation exposing (Key)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter exposing (FilterAtom)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Msg exposing (Msg)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.UniqueId as UniqueId
import Json.Encode as E
import Logger
import Time exposing (Zone)
import View.Select


type alias Model =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueId.Generator
    , log : Logger.History
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


type alias ViewState =
    { configOpen : Bool
    , columnSwappable : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    , selectState : View.Select.State
    , timezone : Zone
    , filterAtomMaterial : FilterAtomMaterial
    }


type alias ColumnSwap =
    { grabbedId : String
    , originalIndex : Int
    , originalOrder : Array String
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , isLocalDevelopment : Bool
    , clientHeight : Int
    }


init : Env -> Key -> Model
init env navKey =
    initModel env navKey


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        { columnStore = ColumnStore.init
        , itemBroker = ItemBroker.init
        , producerRegistry = Producer.initRegistry
        , idGen = UniqueId.init
        , log = Logger.init
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
    , timezone = Time.utc
    , filterAtomMaterial = { ofDiscordChannel = Nothing }
    }


welcomeModel : Env -> Key -> Model
welcomeModel env navKey =
    let
        ( columnStore, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <|
                \newId -> ColumnStore.add (Column.welcome newId) ColumnStore.init
    in
    { columnStore = columnStore
    , itemBroker = ItemBroker.init
    , producerRegistry = Producer.initRegistry
    , idGen = idGen
    , log = Logger.init
    , navKey = navKey
    , viewState = defaultViewState
    , env = env
    }


encodeForPersistence : Model -> E.Value
encodeForPersistence m =
    E.object
        [ ( "columnStore", ColumnStore.encode m.columnStore )
        , ( "itemBroker", Broker.encode Item.encode m.itemBroker )
        , ( "producerRegistry", Producer.encodeRegistry m.producerRegistry )
        , ( "idGen", UniqueId.encodeGenerator m.idGen )
        ]
