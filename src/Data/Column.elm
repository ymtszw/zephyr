module Data.Column exposing (Column, welcome, new, encode, decoder)

{-| Types and functions for columns in Zephyr.


## Types

@docs Column, welcome, new, encode, decoder

-}

import Array exposing (Array)
import Broker exposing (Offset)
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
    , offset : Maybe Offset
    , configOpen : Bool
    , deleteGate : String
    }


encode : Column -> E.Value
encode c =
    E.object
        [ ( "id", E.string c.id )
        , ( "items", E.list Item.encode c.items )
        , ( "filters", E.array Filter.encode c.filters )
        , ( "offset", E.maybe (E.string << Broker.offsetToString) c.offset )
        ]


decoder : Decoder Column
decoder =
    D.map4 (\id items filters offset -> Column id items filters offset False "")
        (D.field "id" D.string)
        (D.field "items" (D.list Item.decoder))
        (D.oneOf
            [ D.field "filters" (D.array Filter.decoder)
            , D.succeed Array.empty -- Migration
            ]
        )
        (D.maybeField "offset" offsetDecoder)


offsetDecoder : Decoder Offset
offsetDecoder =
    D.string
        |> D.andThen
            (\s ->
                case Broker.offsetFromString s of
                    Just offset ->
                        D.succeed offset

                    Nothing ->
                        D.fail ("Invalid Broker.Offset: " ++ s)
            )


welcome : String -> Column
welcome id =
    { id = id
    , items =
        [ Item.welcome
        , Item.textOnly "Source: https://github.com/ymtszw/zephyr\nOutstanding Elm language: https://elm-lang.org"
        ]
    , filters = Array.empty
    , offset = Nothing
    , configOpen = True
    , deleteGate = ""
    }


new : String -> Column
new id =
    { id = id
    , items = [ Item.textOnly "New column created! Let's configure filters above!" ]
    , filters = Array.empty
    , offset = Nothing
    , configOpen = True
    , deleteGate = ""
    }
