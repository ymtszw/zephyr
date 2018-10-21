module Data.Producer.Discord exposing
    ( Discord(..), Guild, Channel, Msg(..), decoder, encode
    , Message, Author(..), Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment, encodeMessage, messageDecoder
    , reload, update, configEl
    , FilterAtomMaterial, imageUrlWithFallback, imageUrlNoFallback, filterAtomMaterial, setChannelFetchStatus
    )

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.


## Types

@docs Discord, Guild, Channel, Msg, decoder, encode


## Message

@docs Message, Author, Embed, EmbedImage, EmbedVideo, EmbedAuthor, Attachment, encodeMessage, messageDecoder


## Component APIs

@docs reload, update, configEl


## Runtime APIs

@docs FilterAtomMaterial, imageUrlWithFallback, imageUrlNoFallback, filterAtomMaterial, setChannelFetchStatus

-}

import Data.ColorTheme exposing (oneDark)
import Data.Filter exposing (FilterAtom(..))
import Data.Producer.Base as Producer exposing (destroy, enter, enterAndFire, yieldAndFire)
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..))
import Dict exposing (Dict)
import Element as El exposing (Color, Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Extra exposing (divMod, ite, setTimeout)
import Html.Attributes
import Http
import HttpExtra as Http
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Octicons
import Task exposing (Task)
import Time exposing (Posix)
import Url exposing (Url)
import View.Parts exposing (disabled, disabledColor, octiconEl, scale12, squareIconEl)



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
    , color : Maybe Color
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


encodeColor : Color -> E.Value
encodeColor color =
    let
        { red, green, blue } =
            El.toRgb color
    in
    E.int (floor red * 0x00010000 + floor green * 0x0100 + floor blue)


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
        , D.tagged "UserAuthor" UserAuthor userDecoder
        , D.tagged "WebhookAuthor" WebhookAuthor userDecoder
        ]


embedDecoder : Decoder Embed
embedDecoder =
    D.map8 Embed
        (D.field "title" (D.maybe D.string))
        (D.field "description" (D.maybe D.string))
        (D.field "url" (D.maybe D.url))
        (D.maybeField "color" colorDecoder)
        (D.maybeField "image" embedImageDecoder)
        (D.maybeField "thumbnail" embedImageDecoder)
        (D.maybeField "video" embedVideoDecoder)
        (D.maybeField "author" embedAuthorDecoder)


colorDecoder : Decoder Color
colorDecoder =
    D.int
        |> D.andThen
            (\int ->
                let
                    ( div256, b ) =
                        divMod 256 int

                    ( r, g ) =
                        divMod 256 div256
                in
                if 0 <= r && r < 256 && 0 <= g && g < 256 && 0 <= b && b < 256 then
                    D.succeed (El.rgb255 r g b)

                else
                    D.fail "Invalid color integer"
            )


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


reload : Producer.Reload Discord Msg
reload discord =
    case discord of
        TokenGiven _ ->
            ( discord, Cmd.none )

        TokenReady token ->
            ( discord, identify token )

        Revisit pov ->
            ( discord, identify pov.token )

        _ ->
            -- Other states should not come from IndexedDB
            reload discord



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


update : Producer.Update Message Discord Msg
update msg discordMaybe =
    case ( msg, discordMaybe ) of
        ( TokenInput str, Just discord ) ->
            enter (tokenInput discord str)

        ( TokenInput str, Nothing ) ->
            enter (TokenGiven str)

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


commitToken : Discord -> Producer.Yield Message Discord Msg
commitToken discord =
    case discord of
        TokenGiven "" ->
            destroy

        TokenGiven token ->
            enterAndFire (TokenReady token) (identify token)

        Hydrated "" _ ->
            destroy

        Hydrated newToken _ ->
            enterAndFire discord (identify newToken)

        Expired "" _ ->
            destroy

        Expired newToken _ ->
            enterAndFire discord (identify newToken)

        _ ->
            -- Otherwise token input is locked; this should not happen
            commitToken discord


handleIdentify : Discord -> User -> Producer.Yield Message Discord Msg
handleIdentify discord user =
    case discord of
        TokenReady token ->
            enterAndFire (Identified (NewSession token user)) (hydrate token)

        Hydrated token pov ->
            detectUserSwitch token pov user

        Expired token pov ->
            detectUserSwitch token pov user

        Revisit pov ->
            -- Successful reload
            startConcurrentFetch (Hydrated pov.token) { pov | user = user }

        Switching _ pov ->
            -- Retried Identify with previous token after error on Switching phase
            detectUserSwitch pov.token pov user

        _ ->
            -- Otherwise Identify should not arrive; just keep state
            enter discord


detectUserSwitch : String -> POV -> User -> Producer.Yield Message Discord Msg
detectUserSwitch token pov user =
    if user.id == pov.user.id then
        -- TODO restart polling timers if token was Expired. Need to check living timers.
        enter (Hydrated token { pov | token = token, user = user })

    else
        enterAndFire (Switching (NewSession token user) pov) (hydrate token)


handleHydrate : Discord -> Dict String Guild -> Dict String Channel -> Producer.Yield Message Discord Msg
handleHydrate discord guilds channels =
    case discord of
        Identified { token, user } ->
            -- Successful register
            startConcurrentFetch (Hydrated token) (POV token user guilds channels)

        Switching { token, user } _ ->
            -- Successful user switch; TODO restart polling timers if token was Expired. Need to check living timers.
            enter (Hydrated token (POV token user guilds channels))

        Rehydrating token pov ->
            -- Not diffing against current POV, just overwrite.
            enter (Hydrated token (POV pov.token pov.user guilds (mergeChannels pov.channels channels)))

        Expired token pov ->
            -- Possibly late arrival.
            enter (Expired token (POV pov.token pov.user guilds (mergeChannels pov.channels channels)))

        _ ->
            -- Otherwise Hydrate should not arrive; just keep state
            enter discord


mergeChannels : Dict String Channel -> Dict String Channel -> Dict String Channel
mergeChannels oldChannels newChannels =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/banned) Channel
            acc

        foundInBoth cId oldChannel newChannel acc =
            -- Using newChannel's last_message_id (can cause skip/dupelication)
            Dict.insert cId { newChannel | fetchStatus = oldChannel.fetchStatus } acc

        foundOnlyInNew cId newChannel acc =
            Dict.insert cId newChannel acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldChannels newChannels Dict.empty


{-| Start concurrent fetches.

All periodic fetches are triggered by concurrent timers.
Maximum total number of timers (= concurrent fetches) is controlled by `fetchConcurrencyFactor`.

This function immediately issues fetches for channels that are `NeverFetched`.

Timers fire `Fetch` msg in relatively small intervals (5 seconds average).
On every `Fetch` event, it searches channel dictionary for "ready-to-fetch" channel and do work if one found.

If a channel is fetched but not having yielded a message,
"next fetch time" is near-exponentially backed off, up to 120 seconds.
If one or more message yielded by a fetch, backoff interval is reset to minimum (5 sec),
thus next few fetches are likely attempted in minimum intervals. Call it "bursting".

TODO If users attempted token (= login user) change, number of timers could be messed up.
There must be some kind of "timer counter" to enforce concurrency amount.

-}
startConcurrentFetch : (POV -> Discord) -> POV -> Producer.Yield Message Discord Msg
startConcurrentFetch stateTagger pov =
    let
        targetChannels =
            Dict.values pov.channels
                |> List.filter (.fetchStatus >> (==) NeverFetched)
                |> List.take fetchConcurrencyFactor
    in
    enterAndFire
        (stateTagger (List.foldl updateChannelBeforeFetch pov targetChannels))
        (Cmd.batch (List.map (fetchOne pov.token) targetChannels |> fillWithTimers fetchConcurrencyFactor))


fetchConcurrencyFactor : Int
fetchConcurrencyFactor =
    5


updateChannelBeforeFetch : Channel -> POV -> POV
updateChannelBeforeFetch targetChannel pov =
    -- This function just transit fetchStatus and update POV, not actually checks the fetchStatus is ready-to-fetch
    case targetChannel.fetchStatus of
        NeverFetched ->
            updateChannel targetChannel.id pov <| \c -> { c | fetchStatus = InitialFetching }

        NextFetchAt posix backoff ->
            updateChannel targetChannel.id pov <| \c -> { c | fetchStatus = Fetching posix backoff }

        _ ->
            pov


updateChannel : String -> POV -> (Channel -> Channel) -> POV
updateChannel cId pov updater =
    { pov | channels = Dict.update cId (Maybe.map updater) pov.channels }


fillWithTimers : Int -> List (Cmd Msg) -> List (Cmd Msg)
fillWithTimers concurrencyFactor cmds =
    cmds ++ List.repeat (concurrencyFactor - List.length cmds) setFetchTimerOne


setFetchTimerOne : Cmd Msg
setFetchTimerOne =
    -- We may randomize interval, but each timer drifts naturally so let it be so.
    setTimeout Fetch 5000


handleRehydrate : Discord -> Producer.Yield Message Discord Msg
handleRehydrate discord =
    case discord of
        Hydrated token pov ->
            -- Rehydrate button should only be available in Hydrated state
            enterAndFire (Rehydrating token pov) (hydrate pov.token)

        _ ->
            enter discord


{-| Handles Fetch event caused by fetch timers.

THIS is where timers are primarily continued/killed.

Timers are:

  - killed if the current token is somehow deemed Expired
  - killed if Fetch event arrived to unexpected states (failsafe, should not happen)
  - otherwise continued

-}
handleFetch : Discord -> Posix -> Producer.Yield Message Discord Msg
handleFetch discord posix =
    case discord of
        Hydrated t pov ->
            fetchOrSetTimer (Hydrated t) pov posix

        Rehydrating t pov ->
            fetchOrSetTimer (Rehydrating t) pov posix

        Expired t pov ->
            -- Effectively killing a timer. Needs to be restarted when new token is regisered.
            enter discord

        Switching newSession pov ->
            fetchOrSetTimer (Switching newSession) pov posix

        _ ->
            -- Timer tick should not arrive in other states
            enter discord


fetchOrSetTimer : (POV -> Discord) -> POV -> Posix -> Producer.Yield Message Discord Msg
fetchOrSetTimer stateTagger pov posix =
    let
        readyToFetchChannels =
            Dict.values pov.channels
                |> List.filter (.fetchStatus >> FetchStatus.lessThan (NextFetchAt posix BO5))
                |> List.sortWith (\a b -> FetchStatus.compare a.fetchStatus b.fetchStatus)
    in
    case readyToFetchChannels of
        [] ->
            enterAndFire (stateTagger pov) setFetchTimerOne

        c :: _ ->
            enterAndFire (stateTagger (updateChannelBeforeFetch c pov)) (fetchOne pov.token c)


handleFetched : Discord -> FetchResult -> Producer.Yield Message Discord Msg
handleFetched discord fetchResult =
    case ( discord, unauthorizedOnFetch fetchResult ) of
        ( Hydrated t pov, False ) ->
            handleFetchedImpl (Hydrated t) pov fetchResult

        ( Hydrated t pov, True ) ->
            enter (Expired t pov)

        ( Rehydrating t pov, False ) ->
            handleFetchedImpl (Rehydrating t) pov fetchResult

        ( Rehydrating t pov, True ) ->
            -- Hydrate may arrive later, but it will be handled by handleHydrate even if the state was Expired
            enter (Expired t pov)

        ( Switching newSession pov, False ) ->
            handleFetchedImpl (Switching newSession) pov fetchResult

        ( Switching newSession pov, True ) ->
            -- Hydrate should be going concurrently; let it handle the case
            enter discord

        ( Expired _ _, _ ) ->
            -- Regardless of its content, discard fetchResult and keep Expired status as is.
            -- If the session is restored with new token later, expects retry from previous lastMessageId
            enter discord

        _ ->
            -- `Fetched` should not arrive in other status
            enter discord


unauthorizedOnFetch : FetchResult -> Bool
unauthorizedOnFetch fetchResult =
    case fetchResult of
        FetchErr _ (Http.BadStatus { status }) ->
            status.code == 401

        _ ->
            False


handleFetchedImpl : (POV -> Discord) -> POV -> FetchResult -> Producer.Yield Message Discord Msg
handleFetchedImpl stateTagger pov fetchResult =
    case fetchResult of
        FetchErr cId _ ->
            nextInitialFetchOrSetTimer [] stateTagger <|
                updateChannel cId pov <|
                    \c ->
                        if forbiddenOnFetch fetchResult then
                            { c | fetchStatus = Forbidden }

                        else if c.fetchStatus == InitialFetching then
                            { c | fetchStatus = NeverFetched }

                        else
                            proceedFetchStatus Nothing [] c

        FetchOk cId ms posix ->
            nextInitialFetchOrSetTimer ms stateTagger <|
                updateChannel cId pov <|
                    proceedFetchStatus (Just posix) ms


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

        ( [], Fetching posix backoff ) ->
            { c | fetchStatus = incrementBackoff backoff (Maybe.withDefault posix posixMaybe) }

        ( m :: _, Fetching posix backoff ) ->
            { c
                | fetchStatus = NextFetchAt (Time.millisToPosix (Time.posixToMillis (Maybe.withDefault posix posixMaybe) + 5000)) BO5
                , lastMessageId = Just (MessageId m.id)
            }

        ( [], _ ) ->
            c

        ( m :: _, _ ) ->
            { c | lastMessageId = Just (MessageId m.id) }


incrementBackoff : Backoff -> Posix -> FetchStatus
incrementBackoff backoff posix =
    case backoff of
        BO5 ->
            NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 5000)) BO10

        BO10 ->
            NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 10000)) BO30

        BO30 ->
            NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 30000)) BO60

        BO60 ->
            NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 60000)) BO120

        BO120 ->
            NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 120000)) BO120


nextFetchWithBaseBackoff : Posix -> FetchStatus
nextFetchWithBaseBackoff posix =
    NextFetchAt (Time.millisToPosix (Time.posixToMillis posix + 5000)) BO5


nextInitialFetchOrSetTimer : List Message -> (POV -> Discord) -> POV -> Producer.Yield Message Discord Msg
nextInitialFetchOrSetTimer items stateTagger pov =
    -- Issues a fetch for NeverFetched channel immediately, otherwise set timer.
    -- XXX Consequently, initial fetching sequence becomes very "bursty", potentially resulting in throttling
    -- Note: reversing items since /messages API sorts messages from latest to oldest
    case List.filter (.fetchStatus >> (==) NeverFetched) (Dict.values pov.channels) of
        [] ->
            yieldAndFire (List.reverse items) (stateTagger pov) setFetchTimerOne

        c :: _ ->
            yieldAndFire (List.reverse items) (stateTagger (updateChannelBeforeFetch c pov)) (fetchOne pov.token c)


handleAPIError : Discord -> Http.Error -> Producer.Yield Message Discord Msg
handleAPIError discord error =
    -- Debug here; mostly, unexpected API errors indicate auth error
    -- TODO log some important errors somehow
    case discord of
        TokenGiven _ ->
            -- Late arrival of API response started in already discarded Discord state? Ignore.
            enter discord

        TokenReady _ ->
            -- Identify failure
            destroy

        Identified _ ->
            -- If successfully Identified, basically Hydrate should not fail. Fall back to token input.
            destroy

        Hydrated _ pov ->
            enter (Hydrated pov.token pov)

        Rehydrating token pov ->
            -- Just fall back to previous Hydrated state.
            enter (Hydrated token pov)

        Revisit pov ->
            -- Identify failure on reload, likely token expiration/revocation
            enter (Expired pov.token pov)

        Expired _ _ ->
            -- Somehow token is expired AND any subsequent API requests also failed. Settle at Expired.
            enter discord

        Switching _ pov ->
            -- Similar to Identified branch (Hydrate after successful Identify should not basically fail).
            -- Directly fall back to previous Hydrated state.
            enter (Hydrated pov.token pov)



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
                    Just ("around=" ++ mId)

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



-- CONFIG VIEW


configEl : Maybe Discord -> Element Msg
configEl discordMaybe =
    case discordMaybe of
        Just discord ->
            tokenFormEl discord

        Nothing ->
            tokenFormEl (TokenGiven "")


tokenFormEl : Discord -> Element Msg
tokenFormEl discord =
    El.column [ El.width El.fill, El.spacing 5 ] <|
        [ Element.Input.text
            (disabled (shouldLockInput discord)
                [ El.width El.fill
                , El.padding 5
                , BG.color oneDark.note
                , BD.width 0
                ]
            )
            { onChange = TokenInput
            , text = tokenText discord
            , placeholder = Nothing
            , label = tokenLabelEl
            }
        , tokenSubmitButtonEl discord
        ]
            ++ currentStateEl discord


tokenSubmitButtonEl : Discord -> Element Msg
tokenSubmitButtonEl discord =
    Element.Input.button
        ([ El.alignRight
         , El.width El.shrink
         , El.padding 10
         , BD.rounded 5
         ]
            |> disabled (shouldLockButton discord)
            |> disabledColor (shouldLockButton discord)
        )
        { onPress = ite (shouldLockButton discord) Nothing (Just CommitToken)
        , label = El.text (tokenInputButtonLabel discord)
        }


shouldLockInput : Discord -> Bool
shouldLockInput discord =
    case discord of
        TokenGiven _ ->
            False

        Hydrated _ _ ->
            False

        Expired _ _ ->
            False

        _ ->
            True


shouldLockButton : Discord -> Bool
shouldLockButton discord =
    case discord of
        TokenGiven "" ->
            True

        TokenGiven _ ->
            False

        Hydrated currentInput pov ->
            -- Prohibit submitting with the same token
            currentInput == pov.token

        Expired currentInput pov ->
            -- Allow submitting with the same token in this case, triggering retry
            False

        _ ->
            True


tokenText : Discord -> String
tokenText discord =
    case discord of
        TokenGiven string ->
            string

        TokenReady string ->
            string

        Identified newSession ->
            newSession.token

        Hydrated token _ ->
            token

        Rehydrating token _ ->
            token

        Switching newSession _ ->
            newSession.token

        Revisit pov ->
            pov.token

        Expired token _ ->
            token


tokenLabelEl : Element.Input.Label msg
tokenLabelEl =
    Element.Input.labelAbove [] <|
        El.column [ El.spacing 5 ]
            [ El.el [] (El.text "Token")
            , El.paragraph [ Font.color oneDark.note, Font.size (scale12 1) ]
                [ El.text "Some shady works required to acquire Discord personal access token. Do not talk about it." ]
            ]


tokenInputButtonLabel : Discord -> String
tokenInputButtonLabel discord =
    case discord of
        TokenGiven _ ->
            "Register"

        TokenReady _ ->
            "Waiting..."

        Identified _ ->
            "Fetching data..."

        Hydrated token _ ->
            if token == "" then
                "Unregister"

            else
                "Change Token"

        Rehydrating _ _ ->
            "Fetching data..."

        Switching _ _ ->
            "Switching Identity"

        Revisit _ ->
            "Reloading..."

        Expired _ _ ->
            "Change Token"


currentStateEl : Discord -> List (Element Msg)
currentStateEl discord =
    case discord of
        Identified newSession ->
            [ userNameAndAvatarEl newSession.user ]

        Hydrated _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            ]

        Rehydrating _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl True pov
            ]

        Revisit pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            ]

        Expired _ pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            ]

        Switching newSession pov ->
            [ userNameAndAvatarEl pov.user
            , guildsEl False pov
            ]

        _ ->
            []


userNameAndAvatarEl : User -> Element Msg
userNameAndAvatarEl user =
    El.row [ El.width El.fill, El.spacing 5 ]
        [ El.el [] (El.text "User: ")
        , El.el
            [ El.width (El.px 32)
            , El.height (El.px 32)
            , BD.rounded 16
            , BG.uncropped (imageUrlWithFallback (Just "32") user.discriminator user.avatar)
            ]
            El.none
        , El.text user.username
        , El.el [ El.centerY, Font.size (scale12 1), Font.color oneDark.note ] (El.text ("#" ++ user.discriminator))
        ]


guildsEl : Bool -> POV -> Element Msg
guildsEl rotating pov =
    El.row [ El.width El.fill, El.spacing 5 ]
        [ El.column [ El.alignTop, El.spacing 5 ]
            [ El.text "Servers: "
            , rehydrateButtonEl rotating pov
            ]
        , pov.guilds
            |> Dict.foldl (\_ guild acc -> guildIconEl guild :: acc) []
            |> El.wrappedRow [ El.width El.fill, El.spacing 5 ]
        ]


guildIconEl : Guild -> Element Msg
guildIconEl guild =
    squareIconEl 50 guild.name (Maybe.map (imageUrlNoFallback (Just "64")) guild.icon)


rehydrateButtonEl : Bool -> POV -> Element Msg
rehydrateButtonEl rotating pov =
    Element.Input.button
        (disabled rotating
            [ El.alignLeft
            , El.height El.fill
            , BD.rounded 30
            , BG.color oneDark.main
            ]
        )
        { onPress = ite rotating Nothing (Just Rehydrate)
        , label = octiconEl Octicons.sync
        }



-- RUNTIME APIs


imageUrlNoFallback : Maybe String -> Image -> String
imageUrlNoFallback sizeMaybe image =
    imageUrlWithFallback sizeMaybe "" (Just image)


imageUrlWithFallback : Maybe String -> String -> Maybe Image -> String
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

        size =
            case sizeMaybe of
                Just sizeStr ->
                    "?size=" ++ sizeStr

                Nothing ->
                    ""
    in
    "https://cdn.discordapp.com" ++ endpoint ++ size


type alias FilterAtomMaterial =
    Maybe ( FilterAtom, Dict String Channel )


filterAtomMaterial : Discord -> FilterAtomMaterial
filterAtomMaterial discord =
    case availablePov discord of
        Just { channels } ->
            let
                filtered =
                    Dict.filter (\_ c -> FetchStatus.isAvailable c.fetchStatus) channels
            in
            case Dict.values filtered of
                [] ->
                    Nothing

                c :: _ ->
                    Just ( OfDiscordChannel c.id, filtered )

        Nothing ->
            Nothing


availablePov : Discord -> Maybe POV
availablePov discord =
    case discord of
        TokenGiven _ ->
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


setChannelFetchStatus : List String -> Discord -> Discord
setChannelFetchStatus subs discord =
    case discord of
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
            discord


setChannelFetchStatusImpl : (POV -> Discord) -> List String -> POV -> Discord
setChannelFetchStatusImpl tagger subs pov =
    let
        newChannels =
            pov.channels
                |> Dict.map
                    (\cId c ->
                        case ( List.member cId subs, FetchStatus.isActive c.fetchStatus ) of
                            ( True, True ) ->
                                c

                            ( True, False ) ->
                                { c | fetchStatus = Waiting }

                            ( False, True ) ->
                                { c | fetchStatus = Available }

                            ( False, False ) ->
                                c
                    )
    in
    tagger { pov | channels = newChannels }
