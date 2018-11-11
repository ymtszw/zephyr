module Data.ItemBroker exposing (bulkAppend, bulkRead, decoder, encode, init, storeId)

import Broker exposing (Broker, Offset)
import Data.Item as Item exposing (Item)
import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


init : Broker Item
init =
    Broker.initialize { numSegments = 100, segmentSize = 1000 }


encode : Broker Item -> Storable
encode itemBroker =
    Data.Storable.encode storeId
        [ ( "broker", Broker.encode Item.encode itemBroker )
        ]


storeId : String
storeId =
    "itemBroker"


decoder : Decoder (Broker Item)
decoder =
    D.field "broker" (Broker.decoder Item.decoder)


{-| Append all Items in a List to a Broker.
Input Items must be ordered from oldest to latest.
-}
bulkAppend : List Item -> Broker Item -> Broker Item
bulkAppend items broker =
    List.foldl Broker.append broker items


{-| Read Broker up to `maxCount` items. Items are ordered from latest to oldest.
-}
bulkRead : Int -> Maybe Offset -> Broker Item -> List ( Item, Offset )
bulkRead maxCount offsetMaybe broker =
    bulkReadImpl offsetMaybe broker maxCount []


bulkReadImpl : Maybe Offset -> Broker Item -> Int -> List ( Item, Offset ) -> List ( Item, Offset )
bulkReadImpl offsetMaybe broker count acc =
    if count <= 0 then
        acc

    else
        case Maybe.withDefault Broker.readOldest (Maybe.map Broker.read offsetMaybe) broker of
            Just ( item, offset ) ->
                bulkReadImpl (Just offset) broker (count - 1) (( item, offset ) :: acc)

            Nothing ->
                acc
