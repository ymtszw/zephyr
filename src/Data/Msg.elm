module Data.Msg exposing (Msg(..))

import Broker exposing (Broker)
import Browser
import Browser.Dom
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
    | LoadColumnStore ( ColumnStore, Cmd Msg )
    | LoadItemBroker (Broker Item)
    | LoadProducerRegistry ProducerRegistry
    | LoadPref Pref
    | LoadErr D.Error
    | ToggleConfig Bool
    | Tick Time.Posix
    | ColumnCtrl ColumnStore.Msg
    | ProducerCtrl ProducerRegistry.Msg
    | PrefCtrl Pref.Msg
    | SelectCtrl (Select.Msg Msg)
    | ModelessCtrl Modeless.Msg
