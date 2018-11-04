module Data.SavedState exposing (SavedState, decoder)

import Broker exposing (Broker)
import Data.Column as Column exposing (Column)
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item as Item exposing (Item)
import Data.ItemBroker as ItemBroker
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.UniqueId as UniqueId
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D


type alias SavedState =
    { columnStore : ColumnStore
    , itemBroker : Broker Item
    , producerRegistry : ProducerRegistry
    , idGen : UniqueId.Generator
    }


decoder : UniqueId.Generator -> Decoder SavedState
decoder idGen =
    -- Write new decoder and migration logic when you change SavedState structure
    D.oneOf
        [ v3StateDecoder
        , v2StateDecoder
        , v1StateDecoder idGen
        ]


v3StateDecoder : Decoder SavedState
v3StateDecoder =
    D.map4 SavedState
        (D.field "columnStore" ColumnStore.decoder)
        (D.maybeField "itemBroker" (Broker.decoder Item.decoder) |> D.map (Maybe.withDefault ItemBroker.init))
        (D.field "producerRegistry" Producer.registryDecoder)
        (D.field "idGen" UniqueId.generatorDecoder)


v2StateDecoder : Decoder SavedState
v2StateDecoder =
    D.map convertFromV2State <|
        D.map2 Tuple.pair
            (D.field "columnStore" ColumnStore.decoder)
            (D.field "idGen" UniqueId.generatorDecoder)


convertFromV2State : ( ColumnStore, UniqueId.Generator ) -> SavedState
convertFromV2State ( columnStore, idGen ) =
    SavedState columnStore ItemBroker.init Producer.initRegistry idGen


v1StateDecoder : UniqueId.Generator -> Decoder SavedState
v1StateDecoder idGen =
    D.field "columns" (D.list Column.decoder)
        |> D.andThen (convertFromV1State idGen)
        |> D.map convertFromV2State


convertFromV1State : UniqueId.Generator -> List Column -> Decoder ( ColumnStore, UniqueId.Generator )
convertFromV1State idGen columns =
    case columns of
        (_ :: _) as nonEmptyColumns ->
            let
                applyId decoded ( accColumnStore, accIdGen ) =
                    UniqueId.genAndMap "column" accIdGen <|
                        \newId ->
                            ColumnStore.add { decoded | id = newId } accColumnStore
            in
            D.succeed <| List.foldr applyId ( ColumnStore.init, idGen ) nonEmptyColumns

        [] ->
            D.fail "No saved columns. Go to fallback."