module Data.Types exposing (Model, Msg(..))

import Array exposing (Array)
import Browser.Dom exposing (Viewport)
import Data.Column exposing (Column)
import Data.UniqueId exposing (Generator)



-- MODEL


type alias Model =
    { columns : Array Column
    , clientHeight : Int
    , idGen : Generator
    }



-- MSG


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Viewport
    | AddColumn
    | DelColumn Int
