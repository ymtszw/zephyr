module Data.Msg exposing (Msg(..))

import Browser
import Browser.Dom
import Data.Column
import Data.Producer
import Json.Decode


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Browser.Dom.Viewport
    | LinkClicked Browser.UrlRequest
    | SelectToggle String Bool
    | SelectPick Msg
    | AddColumn
    | DelColumn Int
    | ToggleColumnSwappable Bool
    | DragStart Int String
    | DragEnter Int
    | DragEnd
    | Load Json.Decode.Value
    | ToggleConfig Bool
    | ToggleColumnConfig String Bool
    | AddColumnFilter String Data.Column.Filter
    | ProducerCtrl Data.Producer.Msg
