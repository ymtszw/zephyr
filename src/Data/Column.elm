module Data.Column exposing (Column, decoder, welcome)

import Data.Item as Item exposing (Item)
import Json.Decode as D exposing (Decoder)


type alias Column =
    { items : List Item
    }


decoder : Decoder Column
decoder =
    D.map Column
        (D.field "items" (D.list Item.decoder))


welcome : Column
welcome =
    { items = List.repeat 1 Item.welcome
    }
