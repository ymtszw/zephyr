module Data.Model exposing
    ( Model, ViewState, Pref, Env, ColumnSwap
    , init, welcome, adjustEvictThreashold
    , encodePref, prefDecoder, prefStoreId, updatePref
    )

{-| Model of the app.

@docs Model, ViewState, Pref, Env, ColumnSwap
@docs init, welcome, adjustEvictThreashold
@docs encodePref, prefDecoder, prefStoreId, updatePref

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
import Data.Storable exposing (Storable)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Logger
import Time exposing (Zone)
import View.Parts exposing (fixedColumnWidth)
import View.Select
import Worque exposing (Work(..), Worque)


type alias Model =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueIdGen
    , pref : Pref
    , worque : Worque
    , log : Logger.History
    , navKey : Key
    , viewState : ViewState
    , env : Env
    }


{-| In "Zephyr mode", Columns are automatically evicted (dismissed)
and reappear when new messages arrived.

`evictThreshold` dictates how many columns can be displayed at a time, in Zephyr mode.
This value is automatically adjusted according to clientWidth.

-}
type alias Pref =
    { zephyrMode : Bool
    , evictThreshold : Int
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
        , producerRegistry = Producer.initRegistry
        , idGen = UniqueIdGen.init
        , pref = defaultPref env.clientWidth
        , worque = Worque.init
        , log = Logger.init
        , navKey = navKey
        , viewState = defaultViewState
        , env = env
        }

    else
        welcome env navKey


defaultPref : Int -> Pref
defaultPref clientWidth =
    { zephyrMode = True
    , evictThreshold = adjustEvictThreashold clientWidth
    }


adjustEvictThreashold : Int -> Int
adjustEvictThreashold clientWidth =
    (clientWidth // fixedColumnWidth) + 1


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
    , pref = defaultPref env.clientWidth
    , log = Logger.init
    , navKey = navKey
    , viewState = defaultViewState
    , env = env
    }


encodePref : Pref -> Storable
encodePref pref =
    Data.Storable.encode prefStoreId
        [ ( "zephyrMode", E.bool pref.zephyrMode )
        ]


prefStoreId : String
prefStoreId =
    "pref"


prefDecoder : Int -> Decoder Pref
prefDecoder clientWidth =
    D.oneOf
        [ D.map2 Pref
            (D.field "zephyrMode" D.bool)
            (D.succeed (adjustEvictThreashold clientWidth))
        , D.succeed (defaultPref clientWidth) -- Casually provide the default, rather than fail on Pref load
        ]


updatePref : Bool -> Model -> ( Model, Bool )
updatePref zephyrMode ({ pref } as m) =
    if m.pref.zephyrMode == zephyrMode then
        ( m, False )

    else
        ( { m | pref = { pref | zephyrMode = zephyrMode } }, True )
