module Data.Msg exposing (Msg(..))

import Array exposing (Array)
import Broker exposing (Broker)
import Browser
import Browser.Dom
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item)
import Data.Pref as Pref exposing (Pref)
import Data.ProducerRegistry as ProducerRegistry exposing (ProducerRegistry)
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
    | LoadColumnStore ( ColumnStore, Cmd Msg )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadPref Pref
    | LoadErr D.Error
    | ToggleConfig Bool
    | ColumnCtrl ColumnStore.Msg
    | ProducerCtrl ProducerRegistry.Msg
    | DomOp (Result Browser.Dom.Error ())
    | PrefCtrl Pref.Msg
    | ModelessTouch Modeless.ModelessId
    | ModelessMove Modeless.ModelessId Int Int
    | ModelessRemove Modeless.ModelessId
    | Tick Time.Posix
