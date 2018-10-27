module Data.Msg exposing (Msg(..))

import Browser
import Browser.Dom
import Data.Filter
import Data.Producer
import Json.Decode
import Logger
import Time exposing (Posix, Zone)


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Browser.Dom.Viewport
    | GetTimeZone ( String, Zone )
    | LoggerCtrl Logger.Msg
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
    | AddColumnFilter String Data.Filter.Filter
    | SetColumnFilter String Int Data.Filter.Filter
    | DelColumnFilter String Int
    | ColumnDeleteGateInput String String
    | ProducerCtrl Data.Producer.Msg
    | ScanBroker Posix
