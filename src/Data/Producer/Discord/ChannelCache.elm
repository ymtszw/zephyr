module Data.Producer.Discord.ChannelCache exposing (ChannelCache, decoder, encode)

import AssocList exposing (Dict)
import Data.Producer.Discord.Channel exposing (Id, Type, decoderShared, encodeShared)
import Data.Producer.Discord.Guild as Guild exposing (Guild)
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value)


{-| Rarely updated part of Channel.
Namely, omitting lastMessageId and fetchStatus.

Used for caching in FilterAtomMaterial.

Using bare Record since it is mainly for View usage.

-}
type alias ChannelCache =
    { id : Id
    , name : String
    , type_ : Type
    , guildMaybe : Maybe Guild
    }


encode : ChannelCache -> Value
encode c =
    encodeShared c []


decoder : Dict Guild.Id Guild -> Decoder ChannelCache
decoder guilds =
    decoderShared ChannelCache guilds
