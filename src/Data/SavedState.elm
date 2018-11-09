module Data.SavedState exposing (SavedState, decoder)

import Broker exposing (Broker)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D


type alias SavedState =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueIdGen
    }


decoder : Int -> UniqueIdGen -> Decoder SavedState
decoder clientHeight idGen =
    -- Write new decoder and migration logic when you change SavedState structure
    D.oneOf
        [ v3StateDecoder clientHeight
        ]


v3StateDecoder : Int -> Decoder SavedState
v3StateDecoder clientHeight =
    D.map4 SavedState
        (D.field "columnStore" (ColumnStore.decoder clientHeight))
        (D.maybeField "itemBroker" (Broker.decoder Item.decoder) |> D.map (Maybe.withDefault ItemBroker.init))
        (D.field "producerRegistry" Producer.registryDecoder)
        (D.field "idGen" UniqueIdGen.generatorDecoder)
