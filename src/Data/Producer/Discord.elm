module Data.Producer.Discord exposing
    ( Discord(..), User, POV, Guild, Channel, ChannelCache, Msg(..), FetchResult(..), decoder, encode, encodeUser
    , Message, Author(..), Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment
    , encodeMessage, messageDecoder, colorDecoder, encodeColor
    , reload, update
    , defaultIconUrl, guildIconOrDefaultUrl, imageUrlWithFallback, imageUrlNoFallback
    , getPov, setChannelFetchStatus, initializing, compareByFetchStatus, unavailableChannel
    )

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.


## Types

@docs Discord, User, POV, Guild, Channel, ChannelCache, Msg, FetchResult, decoder, encode, encodeUser


## Message

@docs Message, Author, Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment
@docs encodeMessage, messageDecoder, colorDecoder, encodeColor


## Component APIs

@docs reload, update


## Runtime APIs

@docs defaultIconUrl, guildIconOrDefaultUrl, imageUrlWithFallback, imageUrlNoFallback
@docs getPov, setChannelFetchStatus, initializing, compareByFetchStatus, unavailableChannel

-}

import Data.Filter exposing (FilterAtom(..))
import Data.Producer.Base as Producer exposing (..)
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Dict exposing (Dict)
import Element
import Hex
import Http
import HttpExtra as Http
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

  - When a user starts filling in token form for the first time, it becomes `TokenGiven` state
  - When the above submitted, changes to `TokenReady`
      - Form is locked while authentication attempted.
  - On successful response from Current User API, it becomes `Identified` with NewSession data.
  - After that, available Guild and Channel lists are retrieved from Discord API.
    When they are all ready, it becomes `Hydrated`.
      - Form is unlocked then.
      - Once fully-hydrated state will be saved to IndexedDB.
      - Message fetching timers start at this point, with fixed amount of concurrency.
  - Upon application reload, it starts with `Revisit` status,
    then become `Hydrated` again if the token successfully re-confirmed.
      - If not, it becomes `Expired` (it could also mean the token is revoked by the server)
  - When token is changed to one for another user, it stops at `Switching` state,
    requesting user confirmation, then move to `Identified`, discarding old Config.
      - TODO Implement confirmation
  - If empty string is submitted as token, the whole state machine is discarded
      - TODO Implement confirmation

-}
type Discord
    = TokenGiven String
    | TokenReady String
    | Identified NewSession
    | ChannelScanning POV
    | Hydrated String POV
    | Rehydrating String POV
    | Revisit POV
    | Expired String POV
    | Switching NewSession POV


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


type alias User =
    { id : String
    , username : String
    , discriminator : String
    , email : Maybe String
    , avatar : Maybe Image
    }


type alias Guild =
    { id : String
    , name : String
    , icon : Maybe Image
    }


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

Used for caching of FilterAtomMaterial.

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
        TokenGiven token ->
            E.tagged "TokenGiven" (E.string token)

        TokenReady token ->
            E.tagged "TokenReady" (E.string token)

        Identified session ->
            -- Step back to TokenReady state for clean retry
            E.tagged "TokenReady" (E.string session.token)

        ChannelScanning pov ->
            E.tagged "ChannelScanning" (encodePov pov)

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
        , ( "guilds", encodeGuildDict pov.guilds )
        , ( "channels", encodeChannelDict pov.channels )
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


encodeGuildDict : Dict String Guild -> E.Value
encodeGuildDict guilds =
    guilds |> Dict.map (\_ v -> encodeGuild v) |> Dict.toList |> E.object


encodeGuild : Guild -> E.Value
encodeGuild guild =
    E.object
        [ ( "id", E.string guild.id )
        , ( "name", E.string guild.name )
        , ( "icon", E.maybe encodeImage guild.icon )
        ]


encodeChannelDict : Dict String Channel -> E.Value
encodeChannelDict channels =
    channels |> Dict.map (\_ v -> encodeChannel v) |> Dict.toList |> E.object


encodeChannel : Channel -> E.Value
encodeChannel channel =
    let
        unwrapMessageId (MessageId mid) =
            mid
    in
    E.object
        [ ( "id", E.string channel.id )
        , ( "name", E.string channel.name )
        , ( "type", encodeChannelType channel.type_ )
        , ( "guild_id", E.maybe E.string (Maybe.map .id channel.guildMaybe) ) -- Only encode guild_id
        , ( "last_message_id", E.maybe (unwrapMessageId >> E.string) channel.lastMessageId ) -- Match field name with Discord's API
        , ( "fetchStatus", FetchStatus.encode channel.fetchStatus )
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
            encodeColor color


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
        , D.tagged "ChannelScanning" ChannelScanning povDecoder
        , D.tagged "TokenReady" TokenReady D.string
        , D.tagged "TokenGiven" TokenGiven D.string

        -- Old versions below; to be removed after migration
        , D.when (D.field "tag" D.string) ((==) "discordRevisit") <|
            D.map Revisit (D.field "pov" povDecoder)
        , D.when (D.field "tag" D.string) ((==) "discordTokenReady") <|
            D.map TokenReady (D.field "token" D.string)
        , D.when (D.field "tag" D.string) ((==) "discordTokenGiven") <|
            D.map TokenGiven (D.field "token" D.string)
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
    let
        populateGuild guildIdMaybe =
            Maybe.andThen (\gId -> Dict.get gId guilds) guildIdMaybe
    in
    D.map6 Channel
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "type" channelTypeDecoder)
        (D.maybeField "guild_id" D.string |> D.map populateGuild)
        (D.maybeField "last_message_id" (D.map MessageId D.string))
        (D.maybeField "fetchStatus" FetchStatus.decoder |> D.map (Maybe.withDefault NeverFetched))


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
        TokenGiven _ ->
            pure discord

        TokenReady token ->
            enterAndFire noop discord (identify token)

        ChannelScanning pov ->
            enterAndFire noop discord (identify pov.token)

        Revisit pov ->
            enterAndFire (PostProcessBase False (calculateFAM pov.channels) Nothing) discord (identify pov.token)

        _ ->
            -- Other state; possibly newly introduced one? Do check its possibility of persistence
            pure discord


calculateFAM : Dict String Channel -> UpdateFAM
calculateFAM channels =
    let
        filtered =
            channels |> Dict.foldl reducer [] |> List.sortWith channelSorter

        reducer _ c acc =
            if FetchStatus.isAvailable c.fetchStatus then
                ChannelCache c.id c.name c.type_ c.guildMaybe :: acc

            else
                acc
    in
    case filtered of
        [] ->
            DestroyFAM

        c :: _ ->
            SetFAM ( OfDiscordChannel c.id, filtered )


channelSorter : ChannelCache -> ChannelCache -> Order
channelSorter a b =
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
    | Fetch Posix
    | Fetched FetchResult
    | APIError Http.Error


type FetchResult
    = FetchErr String Http.Error
    | FetchOk String (List Message) Posix


update : Msg -> Maybe Discord -> Yield
update msg discordMaybe =
    case ( msg, discordMaybe ) of
        ( TokenInput str, Just discord ) ->
            enter noop (tokenInput discord str)

        ( TokenInput str, Nothing ) ->
            enter noop (TokenGiven str)

        ( CommitToken, Just discord ) ->
            commitToken discord

        ( Identify user, Just discord ) ->
            handleIdentify discord user

        ( Hydrate guilds channels, Just discord ) ->
            handleHydrate discord guilds channels

        ( Rehydrate, Just discord ) ->
            handleRehydrate discord

        ( Fetch posix, Just discord ) ->
            handleFetch discord posix

        ( Fetched result, Just discord ) ->
            handleFetched discord result

        ( APIError e, Just discord ) ->
            handleAPIError discord e

        ( _, Nothing ) ->
            -- Timer tick or API response arrived after Discord token is deregistered.
            destroy


tokenInput : Discord -> String -> Discord
tokenInput discord newToken =
    case discord of
        TokenGiven _ ->
            TokenGiven newToken

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
        TokenGiven "" ->
            destroy

        TokenGiven token ->
            enterAndFire noop (TokenReady token) (identify token)

        Hydrated "" _ ->
            destroy

        Hydrated newToken _ ->
            enterAndFire noop discord (identify newToken)

        Expired "" _ ->
            destroy

        Expired newToken _ ->
            enterAndFire noop discord (identify newToken)

        _ ->
            -- Otherwise token input is locked; this should not happen
            commitToken discord


handleIdentify : Discord -> User -> Yield
handleIdentify discord user =
    case discord of
        TokenReady token ->
            enterAndFire (PostProcessBase True KeepFAM Nothing) (Identified (NewSession token user)) (hydrate token)

        Hydrated token pov ->
            detectUserSwitch token pov user

        Expired token pov ->
            detectUserSwitch token pov user

        ChannelScanning pov ->
            -- Successful reload
            startChannelScanning { pov | user = user }

        Revisit pov ->
            -- Successful reload; FAM is already calculated on reload
            -- Currntly we do not auto-Rehydrate on reload since Rehydrate is costly.
            enterAndFire (PostProcessBase True KeepFAM (Just Worque.DiscordFetch)) (Hydrated pov.token { pov | user = user }) Cmd.none

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
        enterAndFire (PostProcessBase True KeepFAM Nothing) (Rehydrating token { pov | token = token, user = user }) (hydrate token)

    else
        enterAndFire (PostProcessBase True KeepFAM Nothing) (Switching (NewSession token user) pov) (hydrate token)


{-| Start initial fetches with some level of concurrency.
-}
startChannelScanning : POV -> Yield
startChannelScanning pov =
    let
        targetChannels =
            Dict.values pov.channels
                |> List.filter (.fetchStatus >> (==) NeverFetched)
                |> List.take initialFetchConcurrency
    in
    case targetChannels of
        [] ->
            enter (PostProcessBase True (calculateFAM pov.channels) (Just Worque.DiscordFetch)) (Hydrated pov.token pov)

        _ ->
            enterAndFire (PostProcessBase True KeepFAM Nothing)
                (ChannelScanning (List.foldl updateChannelBeforeFetch pov targetChannels))
                (Cmd.batch (List.map (fetchOne pov.token) targetChannels))


initialFetchConcurrency : Int
initialFetchConcurrency =
    1


updateChannelBeforeFetch : Channel -> POV -> POV
updateChannelBeforeFetch target pov =
    -- This function just transit fetchStatus and update POV, not actually checks the fetchStatus is ready-to-fetch
    case target.fetchStatus of
        NeverFetched ->
            { pov | channels = Dict.update target.id (Maybe.map (\c -> { c | fetchStatus = InitialFetching })) pov.channels }

        Waiting ->
            { pov | channels = Dict.update target.id (Maybe.map (\c -> { c | fetchStatus = ResumeFetching })) pov.channels }

        NextFetchAt posix backoff ->
            { pov | channels = Dict.update target.id (Maybe.map (\c -> { c | fetchStatus = Fetching posix backoff })) pov.channels }

        _ ->
            pov


handleHydrate : Discord -> Dict String Guild -> Dict String Channel -> Yield
handleHydrate discord guilds channels =
    case discord of
        Identified { token, user } ->
            -- Successful register
            startChannelScanning (POV token user guilds channels)

        Switching { token, user } _ ->
            -- Successful user switch
            startChannelScanning (POV token user guilds channels)

        Rehydrating token pov ->
            -- Re-scan for new channels; existing channels are not fetched here
            startChannelScanning (POV pov.token pov.user guilds (mergeChannels pov.channels channels))

        Expired token pov ->
            -- Possibly late arrival. Not re-scan, but persist.
            enter (PostProcessBase True KeepFAM Nothing) (Expired token (POV pov.token pov.user guilds (mergeChannels pov.channels channels)))

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
            enterAndFire noop (Rehydrating token pov) (hydrate pov.token)

        _ ->
            pure discord


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
                |> List.filter (.fetchStatus >> FetchStatus.lessThan (NextFetchAt posix BO2))
                |> List.sortWith (\a b -> FetchStatus.compare a.fetchStatus b.fetchStatus)
    in
    case readyToFetchChannels of
        [] ->
            enter (PostProcessBase False KeepFAM (Just Worque.DiscordFetch)) (stateTagger pov)

        c :: _ ->
            -- Set next Work on Fetched
            enterAndFire noop (stateTagger (updateChannelBeforeFetch c pov)) (fetchOne pov.token c)


handleFetched : Discord -> FetchResult -> Yield
handleFetched discord fetchResult =
    let
        unsubPersist =
            PostProcessBase True KeepFAM Nothing
    in
    case ( discord, unauthorizedOnFetch fetchResult ) of
        ( ChannelScanning pov, False ) ->
            handleFetchResult continueOrSetTimer pov fetchResult

        ( ChannelScanning pov, True ) ->
            -- This is very unlikely, since the token is quite recently Identified. But possible.
            enter unsubPersist (Expired pov.token pov)

        ( Hydrated t pov, False ) ->
            handleFetchResult (\newPov -> ( Hydrated t newPov, Cmd.none, Nothing )) pov fetchResult

        ( Hydrated t pov, True ) ->
            enter unsubPersist (Expired t pov)

        ( Rehydrating t pov, False ) ->
            -- After successful Hydrate, timer is restarted via ChannelScanning
            handleFetchResult (\newPov -> ( Rehydrating t newPov, Cmd.none, Nothing )) pov fetchResult

        ( Rehydrating t pov, True ) ->
            -- Hydrate may arrive later, but it will be handled by handleHydrate even if the state was Expired
            enter unsubPersist (Expired t pov)

        ( Switching newSession pov, False ) ->
            -- After successful Hydrate, timer is restarted via ChannelScanning
            handleFetchResult (\newPov -> ( Switching newSession newPov, Cmd.none, Nothing )) pov fetchResult

        ( Switching newSession pov, True ) ->
            -- Hydrate should be going concurrently; let it handle the case
            pure discord

        ( Expired _ _, _ ) ->
            -- Regardless of its content, discard fetchResult and keep Expired status as is.
            -- If the session is restored with new token later, expects retry from previous lastMessageId
            pure discord

        _ ->
            -- Othewise we should not receive Fetched; possibly late arrival from discarded state?
            pure discord


unauthorizedOnFetch : FetchResult -> Bool
unauthorizedOnFetch fetchResult =
    case fetchResult of
        FetchErr _ (Http.BadStatus { status }) ->
            status.code == 401

        _ ->
            False


continueOrSetTimer : POV -> ( Discord, Cmd Msg, Maybe UpdateFAM )
continueOrSetTimer newPov =
    case List.filter initializing (Dict.values newPov.channels) of
        [] ->
            ( Hydrated newPov.token newPov, Cmd.none, Just (calculateFAM newPov.channels) )

        cs ->
            case List.filter (\c -> c.fetchStatus == NeverFetched) cs of
                [] ->
                    -- Other InitialFetching channel should resolve soon.
                    ( ChannelScanning newPov, Cmd.none, Nothing )

                c :: _ ->
                    ( ChannelScanning (updateChannelBeforeFetch c newPov), fetchOne newPov.token c, Nothing )


initializing : Channel -> Bool
initializing c =
    c.fetchStatus == NeverFetched || c.fetchStatus == InitialFetching


handleFetchResult : (POV -> ( Discord, Cmd Msg, Maybe UpdateFAM )) -> POV -> FetchResult -> Yield
handleFetchResult finalizer pov fetchResult =
    let
        finalize ms ( updatedPov, updateFAM ) =
            let
                ( newDiscord, cmd, finalFAM ) =
                    finalizer updatedPov
            in
            case ms of
                [] ->
                    enterAndFire (PostProcessBase True (Maybe.withDefault updateFAM finalFAM) (Just Worque.DiscordFetch)) newDiscord cmd

                _ ->
                    -- Note: reversing items since /messages API sorts messages from latest to oldest
                    yieldAndFire (List.reverse ms) (Maybe.withDefault updateFAM finalFAM) (Just Worque.DiscordFetch) newDiscord cmd
    in
    case fetchResult of
        FetchErr cId _ ->
            finalize [] <|
                updateChannel cId pov <|
                    \c ->
                        if forbiddenOnFetch fetchResult then
                            { c | fetchStatus = Forbidden }

                        else if c.fetchStatus == InitialFetching then
                            -- Assuming transient error
                            { c | fetchStatus = NeverFetched }

                        else
                            -- Assuming transient error
                            proceedFetchStatus Nothing [] c

        FetchOk cId ms posix ->
            finalize ms <|
                updateChannel cId pov <|
                    proceedFetchStatus (Just posix) ms


updateChannel : String -> POV -> (Channel -> Channel) -> ( POV, UpdateFAM )
updateChannel cId pov updater =
    case Dict.get cId pov.channels of
        Just c ->
            let
                updated =
                    updater c

                newChannels =
                    Dict.insert cId updated pov.channels

                updateFAM =
                    if c.fetchStatus == InitialFetching then
                        -- FAM will be updated when all channels are fetched. Hold until then.
                        KeepFAM

                    else if updated.fetchStatus == Forbidden then
                        -- Channel permission likely updated; need update FAM
                        calculateFAM newChannels

                    else
                        KeepFAM
            in
            ( { pov | channels = newChannels }, updateFAM )

        Nothing ->
            -- Target Channel somehow gone; deleted?
            ( pov, calculateFAM pov.channels )


forbiddenOnFetch : FetchResult -> Bool
forbiddenOnFetch fetchResult =
    case fetchResult of
        FetchErr _ (Http.BadStatus { status }) ->
            -- Other errors are considered transient, excluding Unauthorized (guarded by unauthorizedOnFetch)
            status.code == 403

        _ ->
            False


proceedFetchStatus : Maybe Posix -> List Message -> Channel -> Channel
proceedFetchStatus posixMaybe ms c =
    -- Messages from /messages API are sorted from latest to oldest
    case ( ms, c.fetchStatus ) of
        ( [], InitialFetching ) ->
            { c | fetchStatus = Available }

        ( m :: _, InitialFetching ) ->
            { c | fetchStatus = Available, lastMessageId = Just (MessageId m.id) }

        ( [], ResumeFetching ) ->
            { c | fetchStatus = NextFetchAt (Maybe.withDefault (posix 0) posixMaybe |> Time.add 2000) BO2 }

        ( m :: _, ResumeFetching ) ->
            { c
                | fetchStatus = NextFetchAt (Maybe.withDefault (posix 0) posixMaybe |> Time.add 2000) BO2
                , lastMessageId = Just (MessageId m.id)
            }

        ( [], Fetching posix backoff ) ->
            { c | fetchStatus = incrementBackoff backoff (Maybe.withDefault posix posixMaybe) }

        ( m :: _, Fetching posix backoff ) ->
            { c
                | fetchStatus = NextFetchAt (Maybe.withDefault posix posixMaybe |> Time.add 2000) BO2
                , lastMessageId = Just (MessageId m.id)
            }

        ( [], _ ) ->
            c

        ( m :: _, _ ) ->
            { c | lastMessageId = Just (MessageId m.id) }


incrementBackoff : Backoff -> Posix -> FetchStatus
incrementBackoff backoff posix =
    case backoff of
        BO2 ->
            NextFetchAt (Time.add 2000 posix) BO5

        BO5 ->
            NextFetchAt (Time.add 5000 posix) BO10

        BO10 ->
            NextFetchAt (Time.add 10000 posix) BO30

        BO30 ->
            NextFetchAt (Time.add 30000 posix) BO60

        BO60 ->
            NextFetchAt (Time.add 60000 posix) BO120

        BO120 ->
            NextFetchAt (Time.add 120000 posix) BO120


handleAPIError : Discord -> Http.Error -> Yield
handleAPIError discord error =
    case discord of
        TokenGiven _ ->
            -- Late arrival of API response started in already discarded Discord state? Ignore.
            pure discord

        TokenReady _ ->
            -- Identify failure
            destroy

        Identified _ ->
            -- If successfully Identified, basically Hydrate should not fail. Fall back to token input.
            destroy

        ChannelScanning pov ->
            -- PROBABLY Identify failure on reload, likely token expiration/revocation
            pure (Expired pov.token pov)

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
    Http.getWithAuth (apiPath "/users/@me" Nothing) (Http.auth token) userDecoder
        |> Http.try Identify APIError


hydrate : String -> Cmd Msg
hydrate token =
    Http.getWithAuth (apiPath "/users/@me/guilds" Nothing) (Http.auth token) decodeGuildArrayIntoDict
        |> Task.andThen (hydrateChannels token)
        |> Http.try identity APIError


decodeGuildArrayIntoDict : Decoder (Dict String Guild)
decodeGuildArrayIntoDict =
    let
        listToDict guildList =
            guildList
                |> List.map (\guild -> ( guild.id, guild ))
                |> Dict.fromList
    in
    D.map listToDict (D.list guildDecoder)


hydrateChannels : String -> Dict String Guild -> Task Http.Error Msg
hydrateChannels token guilds =
    let
        getGuildChannels guildId =
            Http.getWithAuth (apiPath ("/guilds/" ++ guildId ++ "/channels") Nothing)
                (Http.auth token)
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


fetchOne : String -> Channel -> Cmd Msg
fetchOne token channel =
    let
        query =
            case ( channel.fetchStatus, channel.lastMessageId ) of
                ( NeverFetched, Just (MessageId mId) ) ->
                    -- Retrive messages greedily on initial fetch; 100 is the maximum
                    -- <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
                    Just ("limit=100&before=" ++ mId)

                ( NeverFetched, Nothing ) ->
                    Nothing

                ( _, Just (MessageId mId) ) ->
                    Just ("after=" ++ mId)

                _ ->
                    Nothing

        fetchTask =
            -- Note that /messages API returns messages from latest to oldest
            Http.getWithAuth (apiPath ("/channels/" ++ channel.id ++ "/messages") query)
                (Http.auth token)
                (D.leakyList messageDecoder)
    in
    fetchTask
        |> Task.andThen (\ms -> Task.map (FetchOk channel.id ms) Time.now)
        |> Task.mapError (FetchErr channel.id)
        |> Task.attempt
            (\res ->
                case res of
                    Ok ok ->
                        Fetched ok

                    Err err ->
                        Fetched err
            )



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
        TokenGiven _ ->
            Nothing

        TokenReady _ ->
            Nothing

        Identified _ ->
            Nothing

        ChannelScanning pov ->
            Just pov

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


setChannelFetchStatus : List String -> Discord -> ( Discord, Bool )
setChannelFetchStatus subs discord =
    case discord of
        ChannelScanning pov ->
            setChannelFetchStatusImpl ChannelScanning subs pov

        Hydrated t pov ->
            setChannelFetchStatusImpl (Hydrated t) subs pov

        Rehydrating t pov ->
            setChannelFetchStatusImpl (Rehydrating t) subs pov

        Revisit pov ->
            setChannelFetchStatusImpl Revisit subs pov

        Expired t pov ->
            setChannelFetchStatusImpl (Expired t) subs pov

        Switching newSession pov ->
            setChannelFetchStatusImpl (Switching newSession) subs pov

        _ ->
            ( discord, False )


setChannelFetchStatusImpl : (POV -> Discord) -> List String -> POV -> ( Discord, Bool )
setChannelFetchStatusImpl tagger subs pov =
    let
        ( newChannels, shouldPersist ) =
            Dict.foldl reducer ( Dict.empty, False ) pov.channels

        reducer cId c ( accDict, accSP ) =
            Tuple.mapFirst (\newChannel -> Dict.insert cId newChannel accDict) <|
                case ( List.member cId subs, FetchStatus.isActive c.fetchStatus ) of
                    ( True, True ) ->
                        ( c, False )

                    ( True, False ) ->
                        ( { c | fetchStatus = Waiting }, True )

                    ( False, True ) ->
                        ( { c | fetchStatus = Available }, True )

                    ( False, False ) ->
                        ( c, False )
    in
    ( tagger { pov | channels = newChannels }, shouldPersist )


compareByFetchStatus : Channel -> Channel -> Order
compareByFetchStatus a b =
    FetchStatus.compare a.fetchStatus b.fetchStatus
