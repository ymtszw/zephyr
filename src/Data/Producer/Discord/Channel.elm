module Data.Producer.Discord.Channel exposing
    ( Channel, Id, Type, LastMessageId, encode, encodeShared, decoder, decoderShared
    , getId, getName, getType_, getGuildMaybe, getLastMessageId, getFetchStatus
    )

{-| Channel object.

@docs Channel, Id, Type, LastMessageId, encode, encodeShared, decoder, decoderShared
@docs getId, getName, getType_, getGuildMaybe, getLastMessageId, getFetchStatus

-}

import AssocList exposing (Dict)
import Data.Producer.Discord.Guild as Guild exposing (Guild)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


{-| Channel object.

TODO: should includes `recipients` for DMs
<https://discordapp.com/developers/docs/resources/channel#channel-object>

-}
type Channel
    = Channel ChannelRecord


type alias ChannelRecord =
    { id : Id
    , name : String
    , type_ : Type
    , guildMaybe : Maybe Guild -- Can be absent for DMs; not serialized to indexedDB
    , lastMessageId : Maybe LastMessageId

    -- Zephyr-only fields below
    , fetchStatus : FetchStatus
    }


type alias Id =
    Id.Id String Channel


{-| Ignoring voice channel and category.
-}
type Type
    = GuildText
    | DM
    | GroupDM


{-| Separate type in order to avoid cyclic import.
-}
type alias LastMessageId =
    Id.Id String LastMessageIdTag


type LastMessageIdTag
    = LastMessageIdTag



-- Codec


encode : Channel -> E.Value
encode (Channel channel) =
    encodeShared channel <|
        [ ( "last_message_id", E.maybe (Id.encode E.string) channel.lastMessageId )
        , ( "fetchStatus", FetchStatus.encode channel.fetchStatus )
        ]


{-| Can be used for cache too. Guild is unjoined.
-}
encodeShared :
    { x
        | id : Id
        , name : String
        , type_ : Type
        , guildMaybe : Maybe Guild
    }
    -> List ( String, E.Value )
    -> E.Value
encodeShared channel others =
    let
        commonProps =
            [ ( "id", Id.encode E.string channel.id )
            , ( "name", E.string channel.name )
            , ( "type", encodeType channel.type_ )
            , ( "guild_id", E.maybe (Id.encode E.string) (Maybe.map Guild.getId channel.guildMaybe) ) -- Only encode guild_id
            ]
    in
    E.object (commonProps ++ others)


{-| Serialize in the same manner of Discord API.
-}
encodeType : Type -> E.Value
encodeType type_ =
    case type_ of
        GuildText ->
            E.int 0

        DM ->
            E.int 1

        GroupDM ->
            E.int 3


decoder : Dict Guild.Id Guild -> Decoder Channel
decoder guilds =
    -- Here we deliberately ignore last_message_id from Discord API (bare IDs).
    -- That way, FetchStatus can be tidier.
    let
        lastMessageIdDecoder =
            D.oneOf
                [ Id.decoder D.string
                , --Old format
                  D.tagged "MessageId" Id.from D.string
                ]
    in
    decoderShared ChannelRecord guilds
        |> D.andMap (D.maybeField "last_message_id" lastMessageIdDecoder)
        |> D.andMap (D.optionField "fetchStatus" FetchStatus.decoder FetchStatus.Available)
        |> D.map Channel


{-| Can be used for cache too. Guild is joined.
-}
decoderShared : (Id -> String -> Type -> Maybe Guild -> a) -> Dict Guild.Id Guild -> Decoder a
decoderShared mapper guilds =
    D.map4 mapper
        (D.field "id" (Id.decoder D.string))
        (D.field "name" D.string)
        (D.field "type" typeDecoder)
        (D.maybeField "guild_id" (Id.decoder D.string) |> D.map (joinGuild guilds))


joinGuild : Dict Guild.Id Guild -> Maybe Guild.Id -> Maybe Guild
joinGuild guilds guildIdMaybe =
    Maybe.andThen (\gId -> AssocList.get gId guilds) guildIdMaybe


typeDecoder : Decoder Type
typeDecoder =
    D.do D.int <|
        \num ->
            case num of
                0 ->
                    D.succeed GuildText

                1 ->
                    D.succeed DM

                3 ->
                    D.succeed GroupDM

                _ ->
                    -- Ignore voice channels and channel categories.
                    -- Failing here causes entire Channel.decoder to fail.
                    -- Thus it must be used in conjunction with DecodeExtra.leakyList
                    D.fail "Invalid Type"



-- Runtime APIs


getId : Channel -> Id
getId (Channel c) =
    c.id


getName : Channel -> String
getName (Channel c) =
    c.name


getType_ : Channel -> Type
getType_ (Channel c) =
    c.type_


getGuildMaybe : Channel -> Maybe Guild
getGuildMaybe (Channel c) =
    c.guildMaybe


getLastMessageId : Channel -> Maybe LastMessageId
getLastMessageId (Channel c) =
    c.lastMessageId


getFetchStatus : Channel -> FetchStatus
getFetchStatus (Channel c) =
    c.fetchStatus
