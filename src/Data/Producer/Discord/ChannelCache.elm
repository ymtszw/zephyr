module Data.Producer.Discord.ChannelCache exposing
    ( ChannelCache, from, encode, decoder
    , encodeList, listDecoder
    )

{-| Cache of Discord.Channel. Stored in ColumnStore.

@docs ChannelCache, from, encode, decoder
@docs encodeList, listDecoder

-}

import AssocList exposing (Dict)
import Data.Producer.Discord.Channel exposing (..)
import Data.Producer.Discord.Guild as Guild exposing (Guild)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E


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


encodeList : List ChannelCache -> Value
encodeList channelCaches =
    let
        ( encodedChannels, guilds ) =
            unjoinGuildsAndEncodeChannels channelCaches
    in
    E.object
        [ ( "guilds", E.assocList Id.to Guild.encode guilds )
        , ( "channels", E.list identity encodedChannels )
        ]


unjoinGuildsAndEncodeChannels : List ChannelCache -> ( List Value, Dict Guild.Id Guild )
unjoinGuildsAndEncodeChannels channelCaches =
    let
        encodeChannelAndCollectGuild c ( acc, guilds ) =
            Tuple.pair (encode c :: acc) <|
                case c.guildMaybe of
                    Just g ->
                        AssocList.insert (Guild.getId g) g guilds

                    Nothing ->
                        guilds
    in
    -- Notice the order; channelCaches are already sorted, must keep it
    List.foldr encodeChannelAndCollectGuild ( [], AssocList.empty ) channelCaches


decoder : Dict Guild.Id Guild -> Decoder ChannelCache
decoder guilds =
    decoderShared ChannelCache guilds


listDecoder : Decoder (List ChannelCache)
listDecoder =
    D.do (D.field "guilds" (D.assocList Id.from Guild.decoder)) <|
        \guilds ->
            D.field "channels" (D.list (decoder guilds))


from : Channel -> ChannelCache
from c =
    { id = getId c
    , name = getName c
    , type_ = getType_ c
    , guildMaybe = getGuildMaybe c
    }
