module Data.Column exposing (Column, welcome, new, encode, decoder)

{-| Types and functions for columns in Zephyr.


## Types

@docs Column, welcome, new, encode, decoder

-}

import Array exposing (Array)
import Data.Filter as Filter exposing (Filter)
import Data.Item as Item exposing (Item)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


type alias Column =
    { id : String
    , items : List Item
    , filters : Array Filter
    , configOpen : Bool
    , deleteGate : String
    }


decoder : Decoder Column
decoder =
    D.map3
        (\id items filters ->
            { id = id
            , items = items
            , filters = filters
            , configOpen = False
            , deleteGate = ""
            }
        )
        (D.field "id" D.string)
        (D.field "items" (D.list Item.decoder))
        (D.oneOf
            [ D.field "filters" (D.array Filter.decoder)
            , D.succeed Array.empty -- Migration
            ]
        )


encode : Column -> E.Value
encode { id, items, filters } =
    E.object
        [ ( "id", E.string id )
        , ( "items", E.list Item.encode items )
        , ( "filters", E.array Filter.encode filters )
        ]


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ Item.welcome
        , Item.textOnly "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
        ]
    , filters = Array.empty
    , configOpen = True
    , deleteGate = ""
    }


new : String -> Column
new id =
    { id = id
    , items = [ Item.textOnly "New column created! Let's configure filters above!" ]
    , filters = Array.empty
    , configOpen = True
    , deleteGate = ""
    }
