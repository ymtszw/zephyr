module Data.Model exposing
    ( Model, ViewState, Env, ColumnSwap
    , init, addWelcomeColumn
    )

{-| Model of the app.

@docs Model, ViewState, Env, ColumnSwap
@docs init, addWelcomeColumn

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
import Id
import Time exposing (Zone)
import View.Atoms.Input.Select as Select
import View.Organisms.Modeless as Modeless
import Worque exposing (Work(..), Worque)


type alias Model =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , pref : Pref
    , worque : Worque
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
    { grabbedId : Column.Id
    , pinned : Bool
    , originalIndex : Int
    , originalOrder : Array Column.Id
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , isLocalDevelopment : Bool
    , clientWidth : Int
    , clientHeight : Int
    , posix : Int
    }


init : Env -> Key -> Model
init env navKey =
    let
        base =
            { columnStore = ColumnStore.init env.posix
            , itemBroker = ItemBroker.init
            , producerRegistry = ProducerRegistry.init
            , pref = Pref.init env.clientWidth
            , worque = Worque.init
            , navKey = navKey
            , viewState = defaultViewState
            , env = env
            }
    in
    if env.indexedDBAvailable then
        base

    else
        addWelcomeColumn base


defaultViewState : ViewState
defaultViewState =
    { configOpen = False
    , columnSwapMaybe = Nothing
    , selectState = Select.init
    , timezone = Time.utc
    , visible = True
    , modeless = Modeless.init
    }


addWelcomeColumn : Model -> Model
addWelcomeColumn m =
    { m | columnStore = ColumnStore.addWelcome m.env.clientHeight m.columnStore }
