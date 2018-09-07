module Data.Core exposing (Env, Model, Msg(..), initModel, welcomeModel)

import Array exposing (Array)
import Browser.Dom exposing (Viewport)
import Data.Column as Column exposing (Column)
import Data.UniqueId as UniqueId exposing (Generator)
import Json.Decode exposing (Value)



-- MODEL


type alias Model =
    { columns : Array Column
    , idGen : Generator
    , env : Env
    }


type alias Env =
    { serviceWorkerAvailable : Bool
    , indexedDBAvailable : Bool
    , clientHeight : Int
    }


initModel : Env -> Model
initModel env =
    if env.indexedDBAvailable then
        { columns = Array.fromList []
        , idGen = UniqueId.init
        , env = env
        }

    else
        welcomeModel env


welcomeModel : Env -> Model
welcomeModel env =
    let
        ( columns, idGen ) =
            UniqueId.genAndMap "column" UniqueId.init <| \newId -> [ Column.welcome newId ]
    in
    { columns = Array.fromList columns
    , idGen = idGen
    , env = env
    }



-- MSG


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Viewport
    | AddColumn
    | DelColumn Int
    | Load Value
