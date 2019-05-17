module Data.ColumnStore.AvailableSources exposing (AvailableSources, init, encode, decoder)

{-| Data.Column.Sources that available to be used.

Stored in ColumnStore for faster access, act as cache of ProducerRegistry.

@docs AvailableSources, init, encode, decoder

-}

import Data.Column.Source as Source exposing (Source)
import Data.Producer.Discord.ChannelCache as ChannelCache exposing (ChannelCache)
import Data.Producer.Slack.ConvoCache as ConvoCache exposing (ConvoCache)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E


type alias AvailableSources =
    -- Should be sorted by Producers
    { discordChannels : List ChannelCache
    , slackConvos : List ConvoCache
    }


init : AvailableSources
init =
    { discordChannels = []
    , slackConvos = []
    }


encode : AvailableSources -> Value
encode record =
    E.object
        [ ( "discordChannels", ChannelCache.encodeList record.discordChannels )
        , ( "slackConvos", ConvoCache.encodeList record.slackConvos )
        ]


decoder : Decoder AvailableSources
decoder =
    D.map2 AvailableSources
        (D.field "discordChannels" ChannelCache.listDecoder)
        (D.field "slackConvos" ConvoCache.listDecoder)
