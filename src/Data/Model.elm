module Data.Model exposing
    ( Model, ViewState, Env, ColumnSwap
    , init, welcome, encodeForPersistence
    )

{-| Model of the app.

@docs Model, ViewState, Env, ColumnSwap
@docs init, welcome, encodeForPersistence

-}

import Array exposing (Array)
import Broker exposing (Broker)
import Browser.Navigation exposing (Key)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter exposing (FilterAtom)
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Msg exposing (Msg)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Data.Storable
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Encode as E
import Logger
import Time exposing (Zone)
import View.Select
import Worque exposing (Work(..), Worque)


type alias Model =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueIdGen
    , worque : Worque
    , log : Logger.History
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


type alias ViewState =
    { configOpen : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    , selectState : View.Select.State
    , timezone : Zone
    , visible : Bool
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
    if env.indexedDBAvailable then
        { columnStore = ColumnStore.init
        , itemBroker = ItemBroker.init
        , producerRegistry = Producer.initRegistry
        , idGen = UniqueIdGen.init
        , worque = Worque.init
        , log = Logger.init
        , navKey = navKey
        , viewState = defaultViewState
        , env = env
        }

    else
        welcome env navKey


defaultViewState : ViewState
defaultViewState =
    { configOpen = False
    , columnSwapMaybe = Nothing
    , selectState = View.Select.init
    , timezone = Time.utc
    , visible = True
    }


welcome : Env -> Key -> Model
welcome env navKey =
    let
        ( welcomeColumn, finalGen ) =
            UniqueIdGen.init
                |> UniqueIdGen.gen UniqueIdGen.columnPrefix
                |> UniqueIdGen.andThen (\( cId, idGen ) -> Column.welcome env.clientHeight idGen cId)
    in
    { columnStore = ColumnStore.add welcomeColumn ColumnStore.init
    , itemBroker = ItemBroker.init
    , producerRegistry = Producer.initRegistry
    , worque = Worque.init
    , idGen = finalGen
    , log = Logger.init
    , navKey = navKey
    , viewState = defaultViewState
    , env = env
    }


encodeForPersistence : Model -> E.Value
encodeForPersistence m =
    E.object
        [ ( "id", E.string "primary" )
        , ( "columnStore", ColumnStore.encode m.columnStore |> Data.Storable.finalize )
        , ( "itemBroker", Broker.encode Item.encode m.itemBroker )
        , ( "producerRegistry", Producer.encodeRegistry m.producerRegistry |> Data.Storable.finalize )
        , ( "idGen", UniqueIdGen.encode m.idGen )
        ]
