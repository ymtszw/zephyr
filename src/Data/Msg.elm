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
import Data.UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D
import Time
import View.Atoms.Input.Select as Select
import View.Organisms.Modeless as Modeless


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
    | DelColumn Column.Id
    | DismissColumn Int
    | ShowColumn Column.Id
    | DragStart { index : Int, id : Column.Id, pinned : Bool }
    | DragEnter (Array Column.Id)
    | DragEnd
    | LoadColumnStore ( ColumnStore, UniqueIdGen, Cmd Msg )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadPref Pref
    | LoadErr D.Error
    | ToggleConfig Bool
    | ColumnCtrl Column.Id Column.Msg
    | ProducerCtrl ProducerRegistry.Msg
    | RevealColumn Int
    | DomOp (Result Browser.Dom.Error ())
    | PrefCtrl Pref.Msg
    | ModelessTouch Modeless.ModelessId
    | ModelessMove Modeless.ModelessId Int Int
    | ModelessRemove Modeless.ModelessId
    | Tick Time.Posix
