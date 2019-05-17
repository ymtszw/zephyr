module Data.ColumnStore.AvailableSources exposing (AvailableSources, init, encode, decoder, toList)

{-| Data.Column.Sources that available to be used.

Stored in ColumnStore for faster access, act as cache of ProducerRegistry.

@docs AvailableSources, init, encode, decoder, toList

-}

import Data.Column.Source as Source exposing (Source)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E


type alias AvailableSources =
    -- Should be sorted by Producers
    { discordChannels : List Source
    , slackConvos : List Source
    }


init : AvailableSources
init =
    { discordChannels = []
    , slackConvos = []
    }


encode : AvailableSources -> Value
encode record =
    E.object
        [ ( "discordChannels", E.list Source.encode record.discordChannels )
        , ( "slackConvos", E.list Source.encode record.slackConvos )
        ]


decoder : Decoder AvailableSources
decoder =
    D.map2 AvailableSources
        (D.field "discordChannels" (D.list Source.decoder))
        (D.field "slackConvos" (D.list Source.decoder))


toList : AvailableSources -> List Source
toList record =
    record.discordChannels ++ record.slackConvos
