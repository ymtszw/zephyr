module Data.Producer.Discord exposing
    ( Discord(..), User, POV, Guild, Channel, init, decoder, encode, encodeUser
    , ChannelCache, encodeGuild, guildDecoder, encodeChannelCache, channelCacheDecoder
    , Message, Author(..), Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment
    , encodeMessage, messageDecoder, colorDecoder, encodeColor
    , Msg(..), reload, update
    , defaultIconUrl, guildIconOrDefaultUrl, imageUrlWithFallback, imageUrlNoFallback
    , getPov, compareByFetchStatus, unavailableChannel, compareByNames
    )

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.


## Types

@docs Discord, User, POV, Guild, Channel, init, decoder, encode, encodeUser
@docs ChannelCache, encodeGuild, guildDecoder, encodeChannelCache, channelCacheDecoder


## Message

@docs Message, Author, Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment
@docs encodeMessage, messageDecoder, colorDecoder, encodeColor


## Component APIs

@docs Msg, reload, update


## Runtime APIs

@docs defaultIconUrl, guildIconOrDefaultUrl, imageUrlWithFallback, imageUrlNoFallback
@docs getPov, compareByFetchStatus, unavailableChannel, compareByNames

-}

import Data.Filter exposing (FilterAtom(..))
import Data.Producer.Base as Producer exposing (..)
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..), Msg(..))
import Dict exposing (Dict)
import Element
import File exposing (File)
import Hex
import Http
import HttpClient exposing (Error(..))
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import String
import Task exposing (Task)
import Time exposing (Posix)
import TimeExtra as Time exposing (posix)
import Url exposing (Url)
import Worque



-- TYPES


{-| A state machine for Discord that represents authentication status.

  - It starts with `TokenWritable`
  - When the above submitted, changes to `TokenReady`
      - Form is locked while authentication attempted.
  - On successful response from Current User API, it becomes `Identified` with NewSession data.
  - After that, available Guild and Channel lists are retrieved from Discord API.
    When they are all ready, it becomes `Hydrated`.
      - Form is unlocked then.
      - Once fully-hydrated state will be saved to IndexedDB.
  - Upon application reload, it starts with `Revisit` status,
    then become `Hydrated` again if the token successfully re-confirmed.
      - If not, it becomes `Expired` (it could also mean the token is revoked by the server)
  - When token is changed to one for another user, it stops at `Switching` state,
    requesting user confirmation, then move to `Identified`, discarding old Config.
      - TODO Better have multi-account support
  - If empty string is submitted as token, it goes back to `TokenWritable`.
      - TODO Implement discard confirmation

-}
type Discord
    = TokenWritable String
    | TokenReady String
    | Identified NewSession
    | Hydrated String POV
    | Rehydrating String POV
    | Revisit POV
    | Expired String POV
    | Switching NewSession POV


init : Discord
init =
    TokenWritable ""


{-| Current user's point of view.

In Discord, it contains:

  - Working token
  - Login User info
  - ID Dict of subscribing Guilds. May be updated periodically.
  - ID Dict of Channels in subscribing Guilds. Maybe updated periodically

-}
type alias POV =
    { token : String
    , user : User
    , guilds : Dict String Guild
    , channels : Dict String Channel
    }


type alias NewSession =
    { token : String
    , user : User
    }


{-| User object.

Note that there is also Guild Member objects, which contains
Guild-local information of Users such as nicknames.
This might be introduced later.

<https://discordapp.com/developers/docs/resources/user#user-object>

-}
type alias User =
    { id : String
    , username : String
    , discriminator : String
    , email : Maybe String
    , avatar : Maybe Image
    }


{-| Guild object.

<https://discordapp.com/developers/docs/resources/guild#guild-object>

-}
type alias Guild =
    { id : String
    , name : String
    , icon : Maybe Image
    }


{-| Channel object.

<https://discordapp.com/developers/docs/resources/channel#channel-object>

-}
type alias Channel =
    { id : String
    , name : String
    , type_ : ChannelType
    , guildMaybe : Maybe Guild -- Can be absent for DMs; not serialized to indexedDB
    , lastMessageId : Maybe MessageId

    -- Zephyr-only fields below
    , fetchStatus : FetchStatus
    }


{-| Rarely updated part of Channel.
Namely, omitting lastMessageId and fetchStatus.

Used for caching in FilterAtomMaterial.

-}
type alias ChannelCache =
    { id : String
    , name : String
    , type_ : ChannelType
    , guildMaybe : Maybe Guild
    }


unavailableChannel : String -> ChannelCache
unavailableChannel id =
    { id = id
    , name = "(Unavailble)"
    , type_ = GuildText
    , guildMaybe = Nothing
    }


{-| Ignoring voice channel and category.
-}
type ChannelType
    = GuildText
    | DM
    | GroupDM


type MessageId
    = MessageId String


type Image
    = Emoji String
    | GuildIcon { guildId : String, hash : String }
    | UserAvatar { userId : String, hash : String }


{-| Discord Message Object.

Only interested in "type: 0" (DEFAULT) messages.
`guild_id` is somewhat missing, though it can be derived from Channel anyway.

<https://discordapp.com/developers/docs/resources/channel#message-object>

TODO use reactions, with introducing delayed update mechanism

-}
type alias Message =
    { id : String
    , channelId : String
    , author : Author
    , timestamp : Posix
    , content : String
    , embeds : List Embed
    , attachments : List Attachment
    }


type Author
    = UserAuthor User
    | WebhookAuthor User


{-| Embed object.

TODO There are more fields to be added. Some bots rely on them.
<https://discordapp.com/developers/docs/resources/channel#embed-object>

-}
type alias Embed =
    { title : Maybe String
    , description : Maybe String
    , url : Maybe Url
    , color : Maybe Element.Color
    , image : Maybe EmbedImage
    , thumbnail : Maybe EmbedImage -- Embed thumbnail and image are identical in structure
    , video : Maybe EmbedVideo
    , author : Maybe EmbedAuthor
    }


type alias EmbedImage =
    -- XXX Embed can use attachment via attachment ID as url. Might support later.
    { url : Url
    , proxyUrl : Maybe Url
    , height : Maybe Int
    , width : Maybe Int
    }


type alias EmbedVideo =
    { url : Url
    , height : Maybe Int
    , width : Maybe Int
    }


type alias EmbedAuthor =
    { name : String
    , url : Maybe Url
    , iconUrl : Maybe Url
    , proxyIconUrl : Maybe Url
    }


type alias Attachment =
    { filename : String
    , url : Url
    , proxyUrl : Url
    , height : Maybe Int
    , width : Maybe Int
    }



-- ENCODER


encode : Discord -> E.Value
encode discord =
    case discord of
        TokenWritable token ->
            E.tagged "TokenWritable" (E.string token)

        TokenReady token ->
            E.tagged "TokenReady" (E.string token)

        Identified session ->
            -- Step back to TokenReady state for clean retry
            E.tagged "TokenReady" (E.string session.token)

        Hydrated _ pov ->
            E.tagged "Revisit" (encodePov pov)

        Rehydrating _ pov ->
            E.tagged "Revisit" (encodePov pov)

        Switching _ pov ->
            -- Not persisting yet-confirmed UserSession
            E.tagged "Revisit" (encodePov pov)

        Revisit pov ->
            E.tagged "Revisit" (encodePov pov)

        Expired _ pov ->
            E.tagged "Revisit" (encodePov pov)


encodePov : POV -> E.Value
encodePov pov =
    E.object
        [ ( "token", E.string pov.token )
        , ( "user", encodeUser pov.user )
        , ( "guilds", E.dict identity encodeGuild pov.guilds )
        , ( "channels", E.dict identity encodeChannel pov.channels )
        ]


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "id", E.string user.id )
        , ( "username", E.string user.username )
        , ( "discriminator", E.string user.discriminator )
        , ( "email", E.maybe E.string user.email )
        , ( "avatar", E.maybe encodeImage user.avatar )
        ]


encodeImage : Image -> E.Value
encodeImage image =
    case image of
        Emoji id ->
            E.string id

        GuildIcon { hash } ->
            E.string hash

        UserAvatar { hash } ->
            E.string hash


encodeGuild : Guild -> E.Value
encodeGuild guild =
    E.object
        [ ( "id", E.string guild.id )
        , ( "name", E.string guild.name )
        , ( "icon", E.maybe encodeImage guild.icon )
        ]


encodeChannel : Channel -> E.Value
encodeChannel channel =
    E.object <|
        encodeChannelShared channel
            ++ [ ( "last_message_id", E.maybe (\(MessageId mid) -> E.tagged "MessageId" (E.string mid)) channel.lastMessageId )
               , ( "fetchStatus", FetchStatus.encode channel.fetchStatus )
               ]


encodeChannelCache : ChannelCache -> E.Value
encodeChannelCache c =
    E.object (encodeChannelShared c)


encodeChannelShared :
    { x
        | id : String
        , name : String
        , type_ : ChannelType
        , guildMaybe : Maybe Guild
    }
    -> List ( String, E.Value )
encodeChannelShared channel =
    [ ( "id", E.string channel.id )
    , ( "name", E.string channel.name )
    , ( "type", encodeChannelType channel.type_ )
    , ( "guild_id", E.maybe E.string (Maybe.map .id channel.guildMaybe) ) -- Only encode guild_id
    ]


encodeChannelType : ChannelType -> E.Value
encodeChannelType type_ =
    case type_ of
        GuildText ->
            E.int 0

        DM ->
            E.int 1

        GroupDM ->
            E.int 3


encodeMessage : Message -> E.Value
encodeMessage message =
    E.object
        [ ( "id", E.string message.id )
        , ( "channel_id", E.string message.channelId )
        , ( "type", E.int 0 )
        , ( "author", encodeAuthor message.author )
        , ( "timestamp", Iso8601.encode message.timestamp )
        , ( "content", E.string message.content )
        , ( "embeds", E.list encodeEmbed message.embeds )
        , ( "attachments", E.list encodeAttachment message.attachments )
        ]


encodeAuthor : Author -> E.Value
encodeAuthor author =
    case author of
        UserAuthor user ->
            E.tagged "UserAuthor" (encodeUser user)

        WebhookAuthor user ->
            E.tagged "WebhookAuthor" (encodeUser user)


encodeEmbed : Embed -> E.Value
encodeEmbed embed =
    E.object
        [ ( "title", E.maybe E.string embed.title )
        , ( "description", E.maybe E.string embed.description )
        , ( "url", E.maybe E.url embed.url )
        , ( "color", E.maybe encodeColor embed.color )
        , ( "image", E.maybe encodeEmbedImage embed.image )
        , ( "thumbnail", E.maybe encodeEmbedImage embed.thumbnail )
        , ( "video", E.maybe encodeEmbedVideo embed.video )
        , ( "author", E.maybe encodeEmbedAuthor embed.author )
        ]


encodeColor : Element.Color -> E.Value
encodeColor color =
    let
        { red, green, blue } =
            Element.toRgb color

        hex2 =
            floor >> Hex.toString >> String.padLeft 2 '0'
    in
    case Hex.fromString <| hex2 (red * 255) ++ hex2 (green * 255) ++ hex2 (blue * 255) of
        Ok decimal ->
            E.int decimal

        Err _ ->
            -- Should not happen
            E.int 0


encodeEmbedImage : EmbedImage -> E.Value
encodeEmbedImage eImage =
    E.object
        [ ( "url", E.url eImage.url )
        , ( "proxy_url", E.maybe E.url eImage.proxyUrl )
        , ( "height", E.maybe E.int eImage.height )
        , ( "width", E.maybe E.int eImage.width )
        ]


encodeEmbedVideo : EmbedVideo -> E.Value
encodeEmbedVideo eVideo =
    E.object
        [ ( "url", E.url eVideo.url )
        , ( "height", E.maybe E.int eVideo.height )
        , ( "width", E.maybe E.int eVideo.width )
        ]


encodeEmbedAuthor : EmbedAuthor -> E.Value
encodeEmbedAuthor eAuthor =
    E.object
        [ ( "name", E.string eAuthor.name )
        , ( "url", E.maybe E.url eAuthor.url )
        , ( "icon_url", E.maybe E.url eAuthor.iconUrl )
        , ( "proxy_icon_url", E.maybe E.url eAuthor.proxyIconUrl )
        ]


encodeAttachment : Attachment -> E.Value
encodeAttachment attachment =
    E.object
        [ ( "filename", E.string attachment.filename )
        , ( "url", E.string (Url.toString attachment.url) )
        , ( "proxy_url", E.string (Url.toString attachment.proxyUrl) )
        , ( "height", E.maybe E.int attachment.height )
        , ( "width", E.maybe E.int attachment.width )
        ]



-- DECODER


decoder : Decoder Discord
decoder =
    D.oneOf
        [ D.tagged "Revisit" Revisit povDecoder
        , D.tagged "TokenReady" TokenReady D.string
        , D.tagged "TokenWritable" TokenWritable D.string

        -- Old formats
        , D.tagged "ChannelScanning" Revisit povDecoder
        , D.when (D.field "tag" D.string) ((==) "discordRevisit") <|
            D.map Revisit (D.field "pov" povDecoder)
        , D.when (D.field "tag" D.string) ((==) "discordTokenReady") <|
            D.map TokenReady (D.field "token" D.string)

        -- Fallback
        , D.succeed init
        ]


povDecoder : Decoder POV
povDecoder =
    D.field "guilds" (D.dict guildDecoder)
        |> D.andThen
            (\guilds ->
                D.map4 POV
                    (D.field "token" D.string)
                    (D.field "user" userDecoder)
                    (D.succeed guilds)
                    (D.field "channels" (D.dict (channelDecoder guilds)))
            )


userDecoder : Decoder User
userDecoder =
    let
        decodeWithId id =
            D.map4 (User id)
                (D.field "username" D.string)
                (D.field "discriminator" D.string)
                (D.maybeField "email" D.string)
                (D.field "avatar" (D.maybe (D.map (toUserAvatar id) D.string)))

        toUserAvatar id hash =
            UserAvatar { userId = id, hash = hash }
    in
    D.field "id" D.string |> D.andThen decodeWithId


guildDecoder : Decoder Guild
guildDecoder =
    let
        decodeWithId id =
            D.map2 (Guild id)
                (D.field "name" D.string)
                (D.field "icon" (D.maybe (D.map (toGuildIcon id) D.string)))

        toGuildIcon id hash =
            GuildIcon { guildId = id, hash = hash }
    in
    D.field "id" D.string |> D.andThen decodeWithId


channelDecoder : Dict String Guild -> Decoder Channel
channelDecoder guilds =
    -- Here we deliberately ignore last_message_id from Discord API (bare IDs).
    -- That way, FetchStatus can be tidier.
    D.map6 Channel
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "type" channelTypeDecoder)
        (D.maybeField "guild_id" D.string |> D.map (populateGuild guilds))
        (D.maybeField "last_message_id" (D.tagged "MessageId" MessageId D.string))
        (D.maybeField "fetchStatus" FetchStatus.decoder |> D.map (Maybe.withDefault Available))


populateGuild : Dict String Guild -> Maybe String -> Maybe Guild
populateGuild guilds guildIdMaybe =
    Maybe.andThen (\gId -> Dict.get gId guilds) guildIdMaybe


channelCacheDecoder : Dict String Guild -> Decoder ChannelCache
channelCacheDecoder guilds =
    D.map4 ChannelCache
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "type" channelTypeDecoder)
        (D.maybeField "guild_id" D.string |> D.map (populateGuild guilds))


channelTypeDecoder : Decoder ChannelType
channelTypeDecoder =
    D.int
        |> D.andThen
            (\num ->
                case num of
                    0 ->
                        D.succeed GuildText

                    1 ->
                        D.succeed DM

                    3 ->
                        D.succeed GroupDM

                    _ ->
                        D.fail "Invalid ChannelType"
            )


messageDecoder : Decoder Message
messageDecoder =
    -- Only care about DEFAULT message type
    D.when (D.field "type" D.int) ((==) 0) <|
        D.map7 Message
            (D.field "id" D.string)
            (D.field "channel_id" D.string)
            authorDecoder
            (D.field "timestamp" Iso8601.decoder)
            (D.field "content" D.string)
            (D.field "embeds" (D.list embedDecoder))
            (D.field "attachments" (D.list attachmentDecoder))


authorDecoder : Decoder Author
authorDecoder =
    let
        decodeFromDiscordApi =
            D.maybeField "webhook_id" D.string
                |> D.andThen
                    (\webhookMaybe ->
                        D.field "author" userDecoder
                            |> D.map
                                (case webhookMaybe of
                                    Just _ ->
                                        WebhookAuthor

                                    Nothing ->
                                        UserAuthor
                                )
                    )
    in
    D.oneOf
        [ decodeFromDiscordApi
        , D.field "author" <|
            D.oneOf
                [ D.tagged "UserAuthor" UserAuthor userDecoder
                , D.tagged "WebhookAuthor" WebhookAuthor userDecoder
                ]
        ]


embedDecoder : Decoder Embed
embedDecoder =
    D.map8 Embed
        (D.maybeField "title" D.string)
        (D.maybeField "description" D.string)
        (D.maybeField "url" D.url)
        (D.maybeField "color" colorDecoder)
        (D.maybeField "image" embedImageDecoder)
        (D.maybeField "thumbnail" embedImageDecoder)
        (D.maybeField "video" embedVideoDecoder)
        (D.maybeField "author" embedAuthorDecoder)


colorDecoder : Decoder Element.Color
colorDecoder =
    let
        decimalIntToHex =
            Hex.toString >> String.padLeft 6 '0'

        hexToColor hex =
            Result.map3 Element.rgb255
                (hex |> String.slice 0 2 |> Hex.fromString)
                (hex |> String.slice 2 4 |> Hex.fromString)
                (hex |> String.slice 4 6 |> Hex.fromString)
    in
    D.int |> D.andThen (decimalIntToHex >> hexToColor >> D.fromResult "Invalid Color")


embedImageDecoder : Decoder EmbedImage
embedImageDecoder =
    D.map4 EmbedImage
        (D.field "url" D.url)
        (D.maybeField "proxy_url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)


embedVideoDecoder : Decoder EmbedVideo
embedVideoDecoder =
    D.map3 EmbedVideo
        (D.field "url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)


embedAuthorDecoder : Decoder EmbedAuthor
embedAuthorDecoder =
    D.map4 EmbedAuthor
        (D.field "name" D.string)
        (D.maybeField "url" D.url)
        (D.maybeField "icon_url" D.url)
        (D.maybeField "proxy_icon_url" D.url)


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    D.map5 Attachment
        (D.field "filename" D.string)
        (D.field "url" D.url)
        (D.field "proxy_url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)



-- RELOADER


type alias Yield =
    Producer.YieldBase Message ( FilterAtom, List ChannelCache ) Discord Msg


type alias UpdateFAM =
    Producer.UpdateFAM ( FilterAtom, List ChannelCache )


reload : Discord -> Yield
reload discord =
    case discord of
        TokenWritable _ ->
            pure discord

        TokenReady token ->
            enterAndFire ppBase discord (identify token)

        Revisit pov ->
            enterAndFire { ppBase | updateFAM = calculateFAM pov.channels } discord (identify pov.token)

        _ ->
            -- Other state; possibly newly introduced one? Do check its possibility of persistence
            pure discord


calculateFAM : Dict String Channel -> UpdateFAM
calculateFAM channels =
    let
        filtered =
            channels |> Dict.foldl reducer [] |> List.sortWith compareByNames

        reducer _ c acc =
            if FetchStatus.subscribed c.fetchStatus then
                ChannelCache c.id c.name c.type_ c.guildMaybe :: acc

            else
                acc
    in
    case filtered of
        [] ->
            DestroyFAM

        c :: _ ->
            SetFAM ( OfDiscordChannel c.id, filtered )


{-| Order Channels by their Guild names and their own names.

XXX May introduce client position.

-}
compareByNames :
    { x | name : String, guildMaybe : Maybe Guild }
    -> { x | name : String, guildMaybe : Maybe Guild }
    -> Order
compareByNames a b =
    let
        gName =
            -- Tilde is sorted AFTER "z" in ordinary sort algorithms, suitable for fallback
            .guildMaybe >> Maybe.map .name >> Maybe.withDefault "~~~"
    in
    case compare (gName a) (gName b) of
        EQ ->
            compare a.name b.name

        diff ->
            diff



-- UPDATE


type Msg
    = TokenInput String
    | CommitToken
    | Identify User
    | Hydrate (Dict String Guild) (Dict String Channel)
    | Rehydrate
    | Subscribe String
    | Unsubscribe String
    | Fetch Posix
    | Fetched FetchSuccess
    | Post PostOpts
    | Posted PostSuccess
    | ChannelAPIError String HttpClient.Failure
    | GenericAPIError HttpClient.Failure


type alias FetchSuccess =
    { channelId : String, messages : List Message, posix : Posix }


type alias PostOpts =
    { channelId : String, message : Maybe String, file : Maybe File }


type alias PostSuccess =
    { channelId : String, posix : Posix }


update : Msg -> Discord -> Yield
update msg discord =
    case msg of
        TokenInput str ->
            pure (tokenInput discord str)

        CommitToken ->
            commitToken discord

        Identify user ->
            handleIdentify discord user

        Hydrate guilds channels ->
            handleHydrate discord guilds channels

        Rehydrate ->
            handleRehydrate discord

        Subscribe cId ->
            handleSubscribe cId discord

        Unsubscribe cId ->
            handleUnsubscribe cId discord

        Fetch posix ->
            handleFetch discord posix

        Fetched fetchSucc ->
            handleFetched discord fetchSucc

        Post postOpts ->
            handlePost discord postOpts

        Posted postSucc ->
            handlePosted discord postSucc

        ChannelAPIError cId e ->
            handleChannelAPIError cId e discord

        GenericAPIError e ->
            handleGenericAPIError discord e


tokenInput : Discord -> String -> Discord
tokenInput discord newToken =
    case discord of
        TokenWritable _ ->
            TokenWritable newToken

        Hydrated _ pov ->
            Hydrated newToken pov

        Expired _ pov ->
            Expired newToken pov

        _ ->
            -- Committed/just loaded/authenticating token cannot be overwritten until auth attempt resolved
            discord


commitToken : Discord -> Yield
commitToken discord =
    case discord of
        TokenWritable "" ->
            pure init

        TokenWritable token ->
            enterAndFire ppBase (TokenReady token) (identify token)

        Hydrated "" _ ->
            pure init

        Hydrated newToken _ ->
            enterAndFire ppBase discord (identify newToken)

        Expired "" _ ->
            pure init

        Expired newToken _ ->
            enterAndFire ppBase discord (identify newToken)

        _ ->
            -- Otherwise token input is locked; this should not happen
            commitToken discord


handleIdentify : Discord -> User -> Yield
handleIdentify discord user =
    case discord of
        TokenReady token ->
            enterAndFire { ppBase | persist = True } (Identified (NewSession token user)) (hydrate token)

        Hydrated token pov ->
            detectUserSwitch token pov user

        Expired token pov ->
            detectUserSwitch token pov user

        Revisit pov ->
            -- Successful reload; FAM is already calculated on reload
            -- Currntly we do not auto-Rehydrate on reload since Rehydrate is costly.
            enterAndFire { ppBase | persist = True, work = Just Worque.DiscordFetch } (Hydrated pov.token { pov | user = user }) Cmd.none

        Switching _ pov ->
            -- Retried Identify with previous token after error on Switching phase
            detectUserSwitch pov.token pov user

        _ ->
            -- Otherwise Identify should not arrive; just keep state
            pure discord


detectUserSwitch : String -> POV -> User -> Yield
detectUserSwitch token pov user =
    if user.id == pov.user.id then
        -- Go Rehydrate => ChannelScanning => Hydrated route for clean restart
        enterAndFire { ppBase | persist = True } (Rehydrating token { pov | token = token, user = user }) (hydrate token)

    else
        enterAndFire { ppBase | persist = True } (Switching (NewSession token user) pov) (hydrate token)


handleHydrate : Discord -> Dict String Guild -> Dict String Channel -> Yield
handleHydrate discord guilds channels =
    case discord of
        Identified { token, user } ->
            -- Successful register
            enter { persist = True, updateFAM = calculateFAM channels, work = Just Worque.DiscordFetch } <|
                Hydrated token (POV token user guilds channels)

        Switching { token, user } _ ->
            -- Successful user switch
            enter { persist = True, updateFAM = calculateFAM channels, work = Just Worque.DiscordFetch } <|
                Hydrated token (POV token user guilds channels)

        Rehydrating token pov ->
            -- Re-scan for new channels; existing channels are not fetched here
            let
                newChannels =
                    mergeChannels pov.channels channels
            in
            enter { persist = True, updateFAM = calculateFAM newChannels, work = Just Worque.DiscordFetch }
                (Hydrated token { pov | guilds = guilds, channels = newChannels })

        Expired token pov ->
            -- Possibly late arrival. Not re-scan, but persist.
            enter { ppBase | persist = True }
                (Expired token { pov | guilds = guilds, channels = mergeChannels pov.channels channels })

        _ ->
            -- Otherwise Hydrate should not arrive; just keep state
            pure discord


mergeChannels : Dict String Channel -> Dict String Channel -> Dict String Channel
mergeChannels oldChannels newChannels =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/banned) Channel
            -- TODO do equivalent if got 404 with FetchErr
            acc

        foundInBoth cId old new acc =
            -- Use old's last_message_id; let our polling naturally catch up
            Dict.insert cId { new | fetchStatus = old.fetchStatus, lastMessageId = old.lastMessageId } acc

        foundOnlyInNew cId new acc =
            Dict.insert cId new acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldChannels newChannels Dict.empty


handleRehydrate : Discord -> Yield
handleRehydrate discord =
    case discord of
        Hydrated token pov ->
            -- Rehydrate button should only be available in Hydrated state
            enterAndFire ppBase (Rehydrating token pov) (hydrate pov.token)

        _ ->
            pure discord


handleSubscribe : String -> Discord -> Yield
handleSubscribe cId discord =
    case discord of
        Hydrated t pov ->
            subscribeImpl (Hydrated t) cId pov

        Rehydrating t pov ->
            subscribeImpl (Rehydrating t) cId pov

        Switching newSession pov ->
            subscribeImpl (Switching newSession) cId pov

        _ ->
            -- Otherwise you cannot Subscribe
            pure discord


subscribeImpl : (POV -> Discord) -> String -> POV -> Yield
subscribeImpl tagger cId pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs } =
                    FetchStatus.update Sub c.fetchStatus
            in
            -- Not pitching another Worque token; let existing one do the work
            pure (tagger { pov | channels = Dict.insert cId { c | fetchStatus = fs } pov.channels })

        Nothing ->
            -- Channel somehow gone; should not basically happen
            pure (tagger pov)


handleUnsubscribe : String -> Discord -> Yield
handleUnsubscribe cId discord =
    case discord of
        Hydrated t pov ->
            unsubscribeImpl (Hydrated t) cId pov

        Rehydrating t pov ->
            unsubscribeImpl (Rehydrating t) cId pov

        Switching newSession pov ->
            unsubscribeImpl (Switching newSession) cId pov

        Expired t pov ->
            unsubscribeImpl (Expired t) cId pov

        _ ->
            -- Otherwise you cannot Subscribe
            pure discord


unsubscribeImpl : (POV -> Discord) -> String -> POV -> Yield
unsubscribeImpl tagger cId pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs, persist, updateFAM } =
                    FetchStatus.update Unsub c.fetchStatus

                newChannels =
                    Dict.insert cId { c | fetchStatus = fs } pov.channels
            in
            enter { ppBase | persist = persist, updateFAM = updateOrKeepFAM updateFAM newChannels }
                (tagger { pov | channels = newChannels })

        Nothing ->
            pure (tagger pov)


updateOrKeepFAM : Bool -> Dict String Channel -> UpdateFAM
updateOrKeepFAM doUpdate channels =
    if doUpdate then
        calculateFAM channels

    else
        KeepFAM


{-| Handles Fetch event caused by the root timer.
-}
handleFetch : Discord -> Posix -> Yield
handleFetch discord posix =
    case discord of
        Hydrated t pov ->
            fetchOrSkip (Hydrated t) pov posix

        Rehydrating t pov ->
            fetchOrSkip (Rehydrating t) pov posix

        Expired _ _ ->
            -- Effectively unsubscribing from Tick. Needs to be restarted when new token is regisered.
            -- TODO: Error reporting
            pure discord

        Switching newSession pov ->
            fetchOrSkip (Switching newSession) pov posix

        _ ->
            -- Otherwise we are not ready for polling
            pure discord


fetchOrSkip : (POV -> Discord) -> POV -> Posix -> Yield
fetchOrSkip stateTagger pov posix =
    let
        readyToFetchChannels =
            Dict.values pov.channels
                |> List.filter (.fetchStatus >> FetchStatus.lessThan (NextFetchAt posix BO10))
                |> List.sortWith (\a b -> FetchStatus.compare a.fetchStatus b.fetchStatus)
    in
    case readyToFetchChannels of
        [] ->
            enter { ppBase | work = Just Worque.DiscordFetch } (stateTagger pov)

        c :: _ ->
            let
                { fs } =
                    -- We never persist on Start
                    FetchStatus.update (Start posix) c.fetchStatus

                newPov =
                    { pov | channels = Dict.insert c.id { c | fetchStatus = fs } pov.channels }
            in
            -- Set next Work on Fetched
            enterAndFire ppBase (stateTagger newPov) (fetchChannelMessages pov.token c)


handleFetched : Discord -> FetchSuccess -> Yield
handleFetched discord { channelId, messages, posix } =
    case discord of
        Hydrated t pov ->
            updatePovOnFetchSuccess (Hydrated t) channelId messages posix pov

        Rehydrating t pov ->
            updatePovOnFetchSuccess (Rehydrating t) channelId messages posix pov

        Expired t pov ->
            updatePovOnFetchSuccess (Expired t) channelId messages posix pov

        Switching newSession pov ->
            updatePovOnFetchSuccess (Switching newSession) channelId messages posix pov

        _ ->
            -- Should not happen
            pure discord


updatePovOnFetchSuccess : (POV -> Discord) -> String -> List Message -> Posix -> POV -> Yield
updatePovOnFetchSuccess tagger cId ms posix pov =
    case Dict.get cId pov.channels of
        Just c ->
            case ms of
                [] ->
                    let
                        { fs, persist, updateFAM } =
                            FetchStatus.update (Miss posix) c.fetchStatus

                        newChannels =
                            Dict.insert cId { c | fetchStatus = fs } pov.channels
                    in
                    enter
                        { persist = persist
                        , updateFAM = updateOrKeepFAM updateFAM newChannels
                        , work = Just Worque.DiscordFetch
                        }
                        (tagger { pov | channels = newChannels })

                m :: _ ->
                    let
                        { fs, updateFAM } =
                            FetchStatus.update (Hit posix) c.fetchStatus

                        newChannels =
                            -- Discord API returns latest to oldest; save first item's ID in lastMessageId,
                            Dict.insert cId { c | fetchStatus = fs, lastMessageId = Just (MessageId m.id) } pov.channels
                    in
                    -- Then reverse items for post-processing
                    yield (List.reverse ms)
                        (updateOrKeepFAM updateFAM newChannels)
                        (Just Worque.DiscordFetch)
                        (tagger { pov | channels = newChannels })

        Nothing ->
            -- Target Channel somehow gone; deleted?
            enter
                { persist = True
                , updateFAM = calculateFAM pov.channels
                , work = Just Worque.DiscordFetch
                }
                (tagger pov)


handlePost : Discord -> PostOpts -> Yield
handlePost discord opts =
    case discord of
        Hydrated t pov ->
            postOrDiscard (Hydrated t) pov opts

        Rehydrating t pov ->
            postOrDiscard (Rehydrating t) pov opts

        Expired _ _ ->
            -- TODO Error reporting
            pure discord

        Switching newSession pov ->
            postOrDiscard (Switching newSession) pov opts

        _ ->
            pure discord


postOrDiscard : (POV -> Discord) -> POV -> PostOpts -> Yield
postOrDiscard stateTagger pov opts =
    let
        subscribed =
            Dict.get opts.channelId pov.channels
                |> Maybe.map (.fetchStatus >> FetchStatus.subscribed)
                |> Maybe.withDefault False
    in
    if subscribed then
        enterAndFire ppBase (stateTagger pov) (postChannelMessage pov.token opts)

    else
        -- Discard if not subbed; Should not happen
        pure (stateTagger pov)


handlePosted : Discord -> PostSuccess -> Yield
handlePosted discord { channelId, posix } =
    case discord of
        Hydrated t pov ->
            updatePovOnPostSuccess (Hydrated t) channelId posix pov

        Rehydrating t pov ->
            updatePovOnPostSuccess (Rehydrating t) channelId posix pov

        Expired t pov ->
            updatePovOnPostSuccess (Expired t) channelId posix pov

        Switching newSession pov ->
            updatePovOnPostSuccess (Switching newSession) channelId posix pov

        _ ->
            -- Should not happen
            pure discord


updatePovOnPostSuccess : (POV -> Discord) -> String -> Posix -> POV -> Yield
updatePovOnPostSuccess tagger cId posix pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs, persist, updateFAM } =
                    FetchStatus.update (Spur posix) c.fetchStatus

                newChannels =
                    Dict.insert cId { c | fetchStatus = fs } pov.channels
            in
            enter { ppBase | persist = persist, updateFAM = updateOrKeepFAM updateFAM newChannels }
                (tagger { pov | channels = newChannels })

        Nothing ->
            -- Target Channel somehow gone; deleted?
            enter { ppBase | persist = True, updateFAM = calculateFAM pov.channels } (tagger pov)


handleChannelAPIError : String -> HttpClient.Failure -> Discord -> Yield
handleChannelAPIError cId ( httpError, req ) discord =
    case ( discord, httpError ) of
        ( Hydrated t pov, Unauthorized _ ) ->
            enter { ppBase | persist = True } (Expired t pov)

        ( Hydrated t pov, _ ) ->
            updatePovOnChannelAPIError (Hydrated t) cId httpError req pov

        ( Rehydrating t pov, Unauthorized _ ) ->
            enter { ppBase | persist = True } (Expired t pov)

        ( Rehydrating t pov, _ ) ->
            updatePovOnChannelAPIError (Rehydrating t) cId httpError req pov

        ( Expired t pov, Unauthorized _ ) ->
            pure discord

        ( Expired t pov, _ ) ->
            -- Late arrival?
            updatePovOnChannelAPIError (Expired t) cId httpError req pov

        ( Switching newSession pov, Unauthorized _ ) ->
            -- Hydrate should be going at the same time
            pure discord

        ( Switching newSession pov, _ ) ->
            -- Previous POV is still available
            updatePovOnChannelAPIError (Switching newSession) cId httpError req pov

        _ ->
            pure discord


updatePovOnChannelAPIError : (POV -> Discord) -> String -> HttpClient.Error -> HttpClient.Req -> POV -> Yield
updatePovOnChannelAPIError tagger cId httpError req pov =
    case Dict.get cId pov.channels of
        Just c ->
            if channelUnavailable httpError then
                let
                    newChannels =
                        Dict.remove cId pov.channels
                in
                enter
                    { persist = True
                    , updateFAM = calculateFAM newChannels
                    , work = Just Worque.DiscordFetch
                    }
                    (tagger { pov | channels = newChannels })

            else
                -- Considered transient
                let
                    { fs, persist, updateFAM } =
                        FetchStatus.update Fail c.fetchStatus

                    newChannels =
                        Dict.insert cId { c | fetchStatus = fs } pov.channels

                    work =
                        if wasFetchRequest cId req then
                            Just Worque.DiscordFetch

                        else
                            Nothing
                in
                enter
                    { persist = persist
                    , updateFAM = updateOrKeepFAM updateFAM newChannels
                    , work = work
                    }
                    (tagger { pov | channels = newChannels })

        Nothing ->
            -- Channel gone; Error was inevitable
            pure (tagger pov)


channelUnavailable : HttpClient.Error -> Bool
channelUnavailable httpError =
    case httpError of
        Forbidden _ ->
            True

        NotFound _ ->
            True

        _ ->
            False


wasFetchRequest : String -> HttpClient.Req -> Bool
wasFetchRequest cId req =
    req.method == "GET" && String.endsWith (channelMessagesPath cId) req.url.path


handleGenericAPIError : Discord -> HttpClient.Failure -> Yield
handleGenericAPIError discord _ =
    case discord of
        TokenWritable _ ->
            -- Late arrival of API response started in already discarded Discord state? Ignore.
            pure discord

        TokenReady _ ->
            -- Identify failure
            pure init

        Identified _ ->
            -- If successfully Identified, basically Hydrate should not fail. Fall back to token input.
            pure init

        Hydrated _ pov ->
            -- New token was invalid?
            pure (Hydrated pov.token pov)

        Rehydrating token pov ->
            -- Somehow Rehydrate failed. Just fall back to previous Hydrated state. Maybe unauthorized.
            pure (Hydrated token pov)

        Revisit pov ->
            -- Identify failure on reload, likely token expiration/revocation
            pure (Expired pov.token pov)

        Expired _ _ ->
            -- Somehow token is expired AND any subsequent API requests also failed. Settle at Expired.
            pure discord

        Switching _ pov ->
            -- Similar to Identified branch (Hydrate after successful Identify should not basically fail).
            -- Directly fall back to previous Hydrated state.
            pure (Hydrated pov.token pov)



-- REST API CLIENTS


apiPath : String -> Maybe String -> Url
apiPath path queryMaybe =
    { protocol = Url.Https
    , host = "discordapp.com"
    , port_ = Nothing
    , path = "/api" ++ path
    , fragment = Nothing
    , query = queryMaybe
    }


identify : String -> Cmd Msg
identify token =
    HttpClient.getWithAuth (apiPath "/users/@me" Nothing) (HttpClient.auth token) userDecoder
        |> HttpClient.try Identify GenericAPIError


hydrate : String -> Cmd Msg
hydrate token =
    HttpClient.getWithAuth (apiPath "/users/@me/guilds" Nothing) (HttpClient.auth token) decodeGuildArrayIntoDict
        |> Task.andThen (hydrateChannels token)
        |> HttpClient.try identity GenericAPIError


decodeGuildArrayIntoDict : Decoder (Dict String Guild)
decodeGuildArrayIntoDict =
    let
        listToDict guildList =
            guildList
                |> List.map (\guild -> ( guild.id, guild ))
                |> Dict.fromList
    in
    D.map listToDict (D.list guildDecoder)


hydrateChannels : String -> Dict String Guild -> Task HttpClient.Failure Msg
hydrateChannels token guilds =
    let
        getGuildChannels guildId =
            HttpClient.getWithAuth (apiPath ("/guilds/" ++ guildId ++ "/channels") Nothing)
                (HttpClient.auth token)
                (D.leakyList (channelDecoder guilds))

        intoDict listOfChannelList =
            listOfChannelList
                |> List.concatMap (List.map (\channel -> ( channel.id, channel )))
                |> Dict.fromList
    in
    Dict.keys guilds
        |> List.map getGuildChannels
        |> Task.sequence
        |> Task.map (intoDict >> Hydrate guilds)


fetchChannelMessages : String -> Channel -> Cmd Msg
fetchChannelMessages token channel =
    let
        combiner messages posix =
            { channelId = channel.id, messages = messages, posix = posix }
    in
    Task.map2 combiner (fetchChannelMessagesTask token channel) Time.now
        |> HttpClient.try Fetched (ChannelAPIError channel.id)


fetchChannelMessagesTask : String -> Channel -> Task HttpClient.Failure (List Message)
fetchChannelMessagesTask token channel =
    let
        query =
            case channel.lastMessageId of
                -- 100 is the maximum; <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
                Just (MessageId mId) ->
                    Just ("limit=100&after=" ++ mId)

                Nothing ->
                    -- Means never fetched
                    Just "limit=100"
    in
    -- Note that /messages API returns messages from latest to oldest
    HttpClient.getWithAuth (apiPath (channelMessagesPath channel.id) query)
        (HttpClient.auth token)
        (D.leakyList messageDecoder)


channelMessagesPath : String -> String
channelMessagesPath cId =
    "/channels/" ++ cId ++ "/messages"


postChannelMessage : String -> PostOpts -> Cmd Msg
postChannelMessage token { channelId, message, file } =
    let
        postParts =
            List.filterMap identity
                [ Maybe.map (Http.stringPart "content") message
                , Maybe.map (Http.filePart "file") file
                ]

        postTask =
            HttpClient.postFormWithAuth (apiPath (channelMessagesPath channelId) Nothing)
                postParts
                (HttpClient.auth token)
                (D.succeed ())

        combiner () posix =
            { channelId = channelId, posix = posix }
    in
    Task.map2 combiner postTask Time.now
        |> HttpClient.try Posted (ChannelAPIError channelId)



-- RUNTIME APIs


defaultIconUrl : Maybe Int -> String
defaultIconUrl sizeMaybe =
    imageUrlWithFallback sizeMaybe "" Nothing


guildIconOrDefaultUrl : Maybe Int -> Guild -> String
guildIconOrDefaultUrl sizeMaybe g =
    imageUrlWithFallback sizeMaybe "" g.icon


imageUrlNoFallback : Maybe Int -> Image -> String
imageUrlNoFallback sizeMaybe image =
    imageUrlWithFallback sizeMaybe "" (Just image)


imageUrlWithFallback : Maybe Int -> String -> Maybe Image -> String
imageUrlWithFallback sizeMaybe discriminator imageMaybe =
    let
        endpoint =
            case ( imageMaybe, discriminator ) of
                ( Just (Emoji string), _ ) ->
                    "/emojis/" ++ string ++ ".png"

                ( Just (GuildIcon { guildId, hash }), _ ) ->
                    "/icons/" ++ guildId ++ "/" ++ hash ++ ".png"

                ( Just (UserAvatar { userId, hash }), _ ) ->
                    -- This includes Webhook avatar
                    "/avatars/" ++ userId ++ "/" ++ hash ++ ".png"

                ( Nothing, disc ) ->
                    case String.toInt disc of
                        Just int ->
                            "/embed/avatars/" ++ String.fromInt (modBy 5 int) ++ ".png"

                        Nothing ->
                            "/embed/avatars/0.png"

        sizeQuery =
            case sizeMaybe of
                Just size ->
                    "?size=" ++ String.fromInt (imageQuerySize size)

                Nothing ->
                    ""
    in
    "https://cdn.discordapp.com" ++ endpoint ++ sizeQuery


imageQuerySize : Int -> Int
imageQuerySize size =
    if size > 512 then
        1024

    else if size > 256 then
        512

    else if size > 128 then
        256

    else if size > 64 then
        128

    else if size > 32 then
        64

    else if size > 16 then
        32

    else
        16


getPov : Discord -> Maybe POV
getPov discord =
    case discord of
        TokenWritable _ ->
            Nothing

        TokenReady _ ->
            Nothing

        Identified _ ->
            Nothing

        Hydrated _ pov ->
            Just pov

        Rehydrating _ pov ->
            Just pov

        Revisit pov ->
            Just pov

        Expired _ pov ->
            Just pov

        Switching _ pov ->
            Just pov


compareByFetchStatus : Channel -> Channel -> Order
compareByFetchStatus a b =
    FetchStatus.compare a.fetchStatus b.fetchStatus
