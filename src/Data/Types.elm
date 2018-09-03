module Data.Types exposing (Model, Msg(..))

import Browser.Dom exposing (Viewport)
import Data.Column exposing (Column)



-- MODEL


type alias Model =
    { columns : List Column -- FIXME Better use array for shuffling?
    , clientHeight : Int
    }



-- MSG


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Viewport
