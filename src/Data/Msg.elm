module Data.Msg exposing (Msg(..))

import Array exposing (Array)
import Broker exposing (Broker)
import Browser
import Browser.Dom
import Data.Column as Column
import Data.ColumnStore exposing (ColumnStore)
import Data.Filter as Filter
import Data.Item exposing (Item)
import Data.Pref as Pref exposing (Pref)
import Data.ProducerRegistry as ProducerRegistry exposing (ProducerRegistry)
import Data.SavedState exposing (SavedState)
import Data.UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D
import Time
import View.Atoms.Input.Select as Select


type Msg
    = NoOp
    | Resize Int Int
    | GetViewport Browser.Dom.Viewport
    | GetTimeZone ( String, Time.Zone )
    | VisibilityChanged Bool
    | LinkClicked Browser.UrlRequest
    | SelectCtrl (Select.Msg Msg)
    | AddEmptyColumn
    | AddSimpleColumn Filter.FilterAtom
    | DelColumn String
    | DismissColumn Int
    | ShowColumn String
    | DragStart { index : Int, id : String, pinned : Bool }
    | DragEnter (Array String)
    | DragEnd
    | LoadColumnStore ( ColumnStore, UniqueIdGen, Cmd Msg )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadPref Pref
    | LoadOk SavedState
    | LoadErr D.Error
    | ToggleConfig Bool
    | ColumnCtrl String Column.Msg
    | ProducerCtrl ProducerRegistry.Msg
    | RevealColumn Int
    | DomOp (Result Browser.Dom.Error ())
    | PrefCtrl Pref.Msg
    | Tick Time.Posix
