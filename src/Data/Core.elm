module Data.Core exposing (Env, Model, Msg(..), initModel, welcomeModel)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport)
import Browser.Navigation exposing (Key)
import Data.Column as Column exposing (Column)
import Data.UniqueId as UniqueId exposing (Generator)
import Json.Decode exposing (Value)



-- MODEL


type alias Model =
    { columns : Array Column
    , idGen : Generator
    , navKey : Key
    , env : Env
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , clientHeight : Int
    }


initModel : Env -> Key -> Model
initModel env navKey =
    if env.indexedDBAvailable then
        { columns = Array.fromList []
        , idGen = UniqueId.init
        , navKey = navKey
        , env = env
        }

    else
        welcomeModel env navKey


welcomeModel : Env -> Key -> Model
welcomeModel env navKey =
    let
        ( columns, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <| \newId -> [ Column.welcome newId ]
    in
    { columns = Array.fromList columns
    , idGen = idGen
    , navKey = navKey
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
