module Data.ItemBroker exposing (bulkAppend, init, readUpTo50)

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


{-| Read Broker up to 50 items. Items are ordered from latest to oldest.
-}
readUpTo50 : Maybe Offset -> Broker Item -> List ( Item, Offset )
readUpTo50 offsetMaybe broker =
    readUpTo50Impl offsetMaybe broker 50 []


readUpTo50Impl : Maybe Offset -> Broker Item -> Int -> List ( Item, Offset ) -> List ( Item, Offset )
readUpTo50Impl offsetMaybe broker count acc =
    if count <= 0 then
        acc

    else
        case Maybe.withDefault Broker.readOldest (Maybe.map Broker.read offsetMaybe) broker of
            Just ( item, offset ) ->
                readUpTo50Impl (Just offset) broker (count - 1) (( item, offset ) :: acc)

            Nothing ->
                acc
