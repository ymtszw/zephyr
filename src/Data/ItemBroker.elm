module Data.ItemBroker exposing (bulkAppend, bulkRead, init)

import Broker exposing (Broker, Offset)
import Data.Item as Item exposing (Item)


init : Broker Item
init =
    Broker.initialize { numSegments = 100, segmentSize = 1000 }


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
