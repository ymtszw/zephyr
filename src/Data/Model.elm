module Data.Model exposing
    ( Model, ViewState, Env, ColumnSwap
    , init, welcome, defaultHeartrateMillis
    )

{-| Model of the app.

@docs Model, ViewState, Env, ColumnSwap
@docs init, welcome, defaultHeartrateMillis

-}

import Array exposing (Array)
import Broker exposing (Broker)
import Browser.Navigation exposing (Key)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Pref as Pref exposing (Pref)
import Data.ProducerRegistry as ProducerRegistry exposing (ProducerRegistry)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Time exposing (Zone)
import View.Atoms.Input.Select as Select
import View.Organisms.Modeless as Modeless
import Worque exposing (Work(..), Worque)


type alias Model =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueIdGen
    , pref : Pref
    , worque : Worque
    , heartrate : Maybe Float
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


type alias ViewState =
    { configOpen : Bool
    , columnSwapMaybe : Maybe ColumnSwap
    , selectState : Select.State
    , timezone : Zone
    , visible : Bool
    , modeless : Modeless.State
    }


type alias ColumnSwap =
    { grabbedId : String
    , pinned : Bool
    , originalIndex : Int
    , originalOrder : Array String
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , isLocalDevelopment : Bool
    , clientHeight : Int
    , clientWidth : Int
    }


init : Env -> Key -> Model
init env navKey =
    if env.indexedDBAvailable then
        { columnStore = ColumnStore.init
        , itemBroker = ItemBroker.init
        , producerRegistry = ProducerRegistry.init
        , idGen = UniqueIdGen.init
        , pref = Pref.init env.clientWidth
        , worque = Worque.init
        , heartrate = defaultHeartrateMillis
        , navKey = navKey
        , viewState = defaultViewState
        , env = env
        }

    else
        welcome env navKey


defaultHeartrateMillis : Maybe Float
defaultHeartrateMillis =
    -- 10 Hz
    Just 100.0


defaultViewState : ViewState
defaultViewState =
    { configOpen = False
    , columnSwapMaybe = Nothing
    , selectState = Select.init
    , timezone = Time.utc
    , visible = True
    , modeless = Modeless.init
    }


welcome : Env -> Key -> Model
welcome env navKey =
    let
        m =
            init env navKey

        ( welcomeColumn, finalGen ) =
            m.idGen
                |> UniqueIdGen.gen UniqueIdGen.columnPrefix
                |> UniqueIdGen.andThen (\( cId, idGen ) -> Column.welcome env.clientHeight idGen cId)
    in
    { m
        | columnStore = ColumnStore.add Nothing welcomeColumn m.columnStore
        , idGen = finalGen
    }
