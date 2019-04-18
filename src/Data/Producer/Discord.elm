module Data.Producer.Discord exposing
    ( Discord(..), POV, FAM, init, decoder, encode
    , Msg(..), reload, update
    , defaultIconUrl, getPov
    )

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.

@docs Discord, POV, FAM, init, decoder, encode
@docs Msg, reload, update
@docs defaultIconUrl, getPov

-}

import AssocList as Dict exposing (Dict)
import Data.Filter exposing (FilterAtom(..))
import Data.Producer as Producer exposing (..)
import Data.Producer.Discord.Cdn exposing (makeDefaultIconUrl)
import Data.Producer.Discord.Channel as Channel exposing (Channel)
import Data.Producer.Discord.ChannelCache as ChannelCache exposing (ChannelCache)
import Data.Producer.Discord.Guild as Guild exposing (Guild)
import Data.Producer.Discord.Message as Message exposing (Message)
import Data.Producer.Discord.User as User exposing (User)
import Data.Producer.FetchStatus as FetchStatus exposing (Backoff(..), FetchStatus(..), Msg(..))
import File exposing (File)
import Http
import HttpClient exposing (Error(..))
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Task exposing (Task)
import Time exposing (Posix)
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
    = TokenWritable Token
    | TokenReady Token
    | Identified NewSession
    | Hydrated Token POV
    | Rehydrating Token POV
    | Revisit POV
    | Expired Token POV
    | Switching NewSession POV


init : Discord
init =
    TokenWritable (Token "")


type Token
    = Token String


fromToken : Token -> String
fromToken (Token t) =
    t


{-| Current user's point of view.

In Discord, it contains:

  - Working token
  - Login User info
  - ID Dict of subscribing Guilds. May be updated periodically.
  - ID Dict of Channels in subscribing Guilds. Maybe updated periodically

-}
type alias POV =
    { token : Token
    , user : User
    , guilds : Dict Guild.Id Guild
    , channels : Dict Channel.Id Channel
    }


type alias NewSession =
    { token : Token
    , user : User
    }


type alias FAM =
    -- List instead of Dict, should be sorted already
    ( FilterAtom, List ChannelCache )



-- ENCODER


encode : Discord -> E.Value
encode discord =
    case discord of
        TokenWritable token ->
            E.tagged "TokenWritable" (E.string (fromToken token))

        TokenReady token ->
            E.tagged "TokenReady" (E.string (fromToken token))

        Identified session ->
            -- Step back to TokenReady state for clean retry
            E.tagged "TokenReady" (E.string (fromToken session.token))

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
        [ ( "token", E.string (fromToken pov.token) )
        , ( "user", User.encode pov.user )
        , ( "guilds", E.assocList Id.to Guild.encode pov.guilds )
        , ( "channels", E.assocList Id.to Channel.encode pov.channels )
        ]


encodeFam : FAM -> E.Value
encodeFam ( defaultAtom, channelCaches ) =
    let
        ( encodedChannels, guilds ) =
            unjoinGuildsAndEncodeChannels channelCaches
    in
    E.object
        [ ( "default", Data.Filter.encodeFilterAtom defaultAtom )
        , ( "guilds", E.assocList Id.to Guild.encode guilds )
        , ( "channels", E.list identity encodedChannels )
        ]


unjoinGuildsAndEncodeChannels : List ChannelCache -> ( List E.Value, Dict Guild.Id Guild )
unjoinGuildsAndEncodeChannels channelCaches =
    let
        encodeChannelAndCollectGuild c ( acc, guilds ) =
            Tuple.pair (ChannelCache.encode c :: acc) <|
                case c.guildMaybe of
                    Just g ->
                        Dict.insert (Guild.getId g) g guilds

                    Nothing ->
                        guilds
    in
    -- Notice the order; channelCaches are already sorted, must keep it
    List.foldr encodeChannelAndCollectGuild ( [], Dict.empty ) channelCaches



-- DECODER


decoder : Decoder Discord
decoder =
    D.oneOf
        [ D.tagged "Revisit" Revisit povDecoder
        , D.tagged "TokenReady" TokenReady (D.map Token D.string)
        , D.tagged "TokenWritable" TokenWritable (D.map Token D.string)

        -- Old formats
        , D.tagged "ChannelScanning" Revisit povDecoder
        , D.when (D.field "tag" D.string) ((==) "discordRevisit") <|
            D.map Revisit (D.field "pov" povDecoder)
        , D.when (D.field "tag" D.string) ((==) "discordTokenReady") <|
            D.map TokenReady (D.field "token" (D.map Token D.string))

        -- Fallback
        , D.succeed init
        ]


povDecoder : Decoder POV
povDecoder =
    D.do (D.field "guilds" (D.assocList Id.from Guild.decoder)) <|
        \guilds ->
            D.map4 POV
                (D.field "token" (D.map Token D.string))
                (D.field "user" User.decoder)
                (D.succeed guilds)
                (D.field "channels" (D.assocList Id.from (Channel.decoder guilds)))


famDecoder : Decoder FAM
famDecoder =
    D.do (D.field "default" Data.Filter.filterAtomDecoder) <|
        \fa ->
            D.do (D.field "guilds" (D.assocList Id.from Guild.decoder)) <|
                \guilds ->
                    D.do (D.field "channels" (D.list (ChannelCache.decoder guilds))) <|
                        \channelCaches ->
                            D.succeed ( fa, channelCaches )



-- RELOADER


type alias Yield =
    Producer.Yield Message FAM Msg


reload : Discord -> ( Discord, Yield )
reload discord =
    case discord of
        TokenWritable _ ->
            pure discord

        TokenReady token ->
            ( discord, { yield | cmd = identify token } )

        Revisit pov ->
            ( discord, { yield | cmd = identify pov.token, updateFAM = calculateFAM pov.channels } )

        _ ->
            -- Other state; possibly newly introduced one? Do check its possibility of persistence
            pure discord


calculateFAM : Dict Channel.Id Channel -> Producer.UpdateFAM FAM
calculateFAM channels =
    let
        filtered =
            channels |> Dict.foldl reducer [] |> List.sortWith Channel.compareShared

        reducer _ c acc =
            if FetchStatus.subscribed (Channel.getFetchStatus c) then
                ChannelCache.from c :: acc

            else
                acc
    in
    case filtered of
        [] ->
            DestroyFAM

        c :: _ ->
            SetFAM ( OfDiscordChannel c.id, filtered )



-- UPDATE


type Msg
    = TokenInput String
    | TokenCommit
    | Identify User
    | Hydrate (Dict Guild.Id Guild) (Dict Channel.Id Channel)
    | Rehydrate
    | Subscribe Channel.Id
    | Unsubscribe Channel.Id
    | Fetch Posix
    | Fetched FetchSuccess
    | Post PostOpts
    | Posted PostSuccess
    | ChannelAPIError Channel.Id HttpClient.Failure
    | GenericAPIError HttpClient.Failure


type alias FetchSuccess =
    { channelId : Channel.Id
    , messages : List Message
    , posix : Posix
    }


type alias PostOpts =
    { channelId : Channel.Id
    , message : Maybe String
    , file : Maybe File
    }


type alias PostSuccess =
    { channelId : Channel.Id
    , posix : Posix
    }


update : Msg -> Discord -> ( Discord, Yield )
update msg discord =
    case msg of
        TokenInput str ->
            pure (tokenInput discord str)

        TokenCommit ->
            tokenCommit discord

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
            TokenWritable (Token newToken)

        Hydrated _ pov ->
            Hydrated (Token newToken) pov

        Expired _ pov ->
            Expired (Token newToken) pov

        _ ->
            -- Committed/just loaded/authenticating token cannot be overwritten until auth attempt resolved
            discord


tokenCommit : Discord -> ( Discord, Yield )
tokenCommit discord =
    case discord of
        TokenWritable (Token "") ->
            pure init

        TokenWritable token ->
            ( TokenReady token, { yield | cmd = identify token } )

        Hydrated (Token "") _ ->
            pure init

        Hydrated newToken _ ->
            ( discord, { yield | cmd = identify newToken } )

        Expired (Token "") _ ->
            pure init

        Expired newToken _ ->
            ( discord, { yield | cmd = identify newToken } )

        _ ->
            -- Otherwise token input is locked; this should not happen
            pure discord


handleIdentify : Discord -> User -> ( Discord, Yield )
handleIdentify discord user =
    case discord of
        TokenReady token ->
            ( Identified (NewSession token user), { yield | cmd = hydrate token, persist = True } )

        Hydrated token pov ->
            detectUserSwitch token pov user

        Expired token pov ->
            detectUserSwitch token pov user

        Revisit pov ->
            -- Successful reload; FAM is already calculated on reload
            -- Currntly we do not auto-Rehydrate on reload since Rehydrate is costly.
            ( Hydrated pov.token { pov | user = user }, { yield | persist = True, work = Just Worque.DiscordFetch } )

        Switching _ pov ->
            -- Retried Identify with previous token after error on Switching phase
            detectUserSwitch pov.token pov user

        _ ->
            -- Otherwise Identify should not arrive; just keep state
            pure discord


detectUserSwitch : Token -> POV -> User -> ( Discord, Yield )
detectUserSwitch token pov user =
    if User.getId user == User.getId pov.user then
        -- Go Rehydrate => Rehydrating => Hydrated route for clean restart
        ( Rehydrating token { pov | token = token, user = user }, { yield | cmd = hydrate token, persist = True } )

    else
        ( Switching (NewSession token user) pov, { yield | cmd = hydrate token, persist = True } )


handleHydrate : Discord -> Dict Guild.Id Guild -> Dict Channel.Id Channel -> ( Discord, Yield )
handleHydrate discord guilds channels =
    case discord of
        Identified { token, user } ->
            -- Successful register
            ( Hydrated token (POV token user guilds channels)
            , { yield | persist = True, updateFAM = calculateFAM channels, work = Just Worque.DiscordFetch }
            )

        Switching { token, user } _ ->
            -- Successful user switch; XXX we may not need to put Worque.DiscordFetch
            ( Hydrated token (POV token user guilds channels)
            , { yield | persist = True, updateFAM = calculateFAM channels, work = Just Worque.DiscordFetch }
            )

        Rehydrating token pov ->
            let
                newChannels =
                    mergeChannels pov.channels channels
            in
            ( Hydrated token { pov | guilds = guilds, channels = newChannels }
            , { yield | persist = True, updateFAM = calculateFAM newChannels }
            )

        Expired token pov ->
            -- Possibly late arrival. Not re-scan, but persist.
            ( Expired token { pov | guilds = guilds, channels = mergeChannels pov.channels channels }
            , { yield | persist = True }
            )

        _ ->
            -- Otherwise Hydrate should not arrive; just keep state
            pure discord


mergeChannels : Dict Channel.Id Channel -> Dict Channel.Id Channel -> Dict Channel.Id Channel
mergeChannels oldChannels newChannels =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/banned) Channel
            acc

        foundInBoth cId old new acc =
            -- Use old's last_message_id; let our polling naturally catch up
            let
                modified =
                    new
                        |> Channel.setFetchStatus (Channel.getFetchStatus old)
                        |> Channel.setLastMessageId (Channel.getLastMessageId old)
            in
            Dict.insert cId modified acc

        foundOnlyInNew cId new acc =
            Dict.insert cId new acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldChannels newChannels Dict.empty


handleRehydrate : Discord -> ( Discord, Yield )
handleRehydrate discord =
    case discord of
        Hydrated token pov ->
            -- Rehydrate button should only be available in Hydrated state
            ( Rehydrating token pov, { yield | cmd = hydrate pov.token } )

        _ ->
            pure discord


handleSubscribe : Channel.Id -> Discord -> ( Discord, Yield )
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


subscribeImpl : (POV -> Discord) -> Channel.Id -> POV -> ( Discord, Yield )
subscribeImpl tagger cId pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs } =
                    FetchStatus.update Sub (Channel.getFetchStatus c)
            in
            -- Not pitching another Worque token; let existing one do the work
            pure (tagger { pov | channels = Dict.insert cId (Channel.setFetchStatus fs c) pov.channels })

        Nothing ->
            -- Channel somehow gone; should not basically happen
            pure (tagger pov)


handleUnsubscribe : Channel.Id -> Discord -> ( Discord, Yield )
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


unsubscribeImpl : (POV -> Discord) -> Channel.Id -> POV -> ( Discord, Yield )
unsubscribeImpl tagger cId pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs, persist, updateFAM } =
                    FetchStatus.update Unsub (Channel.getFetchStatus c)

                newChannels =
                    Dict.insert cId (Channel.setFetchStatus fs c) pov.channels
            in
            ( tagger { pov | channels = newChannels }
            , { yield | persist = persist, updateFAM = updateOrKeepFAM updateFAM newChannels }
            )

        Nothing ->
            pure (tagger pov)


updateOrKeepFAM : Bool -> Dict Channel.Id Channel -> Producer.UpdateFAM FAM
updateOrKeepFAM doUpdate channels =
    if doUpdate then
        calculateFAM channels

    else
        KeepFAM


{-| Handles Fetch event caused by the root timer.
-}
handleFetch : Discord -> Posix -> ( Discord, Yield )
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


fetchOrSkip : (POV -> Discord) -> POV -> Posix -> ( Discord, Yield )
fetchOrSkip stateTagger pov posix =
    let
        readyToFetchChannels =
            Dict.values pov.channels
                |> List.filter (Channel.getFetchStatus >> FetchStatus.lessThan (NextFetchAt posix BO10))
                |> List.sortWith (\a b -> FetchStatus.compare (Channel.getFetchStatus a) (Channel.getFetchStatus b))
    in
    case readyToFetchChannels of
        [] ->
            ( stateTagger pov, { yield | work = Just Worque.DiscordFetch } )

        c :: _ ->
            let
                { fs } =
                    -- We never persist on Start
                    FetchStatus.update (Start posix) (Channel.getFetchStatus c)

                newPov =
                    { pov | channels = Dict.insert (Channel.getId c) (Channel.setFetchStatus fs c) pov.channels }
            in
            -- Set next Work on Fetched
            ( stateTagger newPov, { yield | cmd = fetchChannelMessages pov.token c } )


handleFetched : Discord -> FetchSuccess -> ( Discord, Yield )
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


updatePovOnFetchSuccess : (POV -> Discord) -> Channel.Id -> List Message -> Posix -> POV -> ( Discord, Yield )
updatePovOnFetchSuccess tagger cId ms posix pov =
    case Dict.get cId pov.channels of
        Just c ->
            case ms of
                [] ->
                    let
                        { fs, persist, updateFAM } =
                            FetchStatus.update (Miss posix) (Channel.getFetchStatus c)

                        newChannels =
                            Dict.insert cId (Channel.setFetchStatus fs c) pov.channels
                    in
                    ( tagger { pov | channels = newChannels }
                    , { yield
                        | persist = persist
                        , updateFAM = updateOrKeepFAM updateFAM newChannels
                        , work = Just Worque.DiscordFetch
                      }
                    )

                m :: _ ->
                    let
                        { fs, updateFAM } =
                            FetchStatus.update (Hit posix) (Channel.getFetchStatus c)

                        newChannels =
                            -- Discord API returns latest to oldest; save first item's ID in lastMessageId,
                            let
                                c_ =
                                    c
                                        |> Channel.setLastMessageId (Just (Message.getId m))
                                        |> Channel.setFetchStatus fs
                            in
                            Dict.insert cId c_ pov.channels
                    in
                    -- Then reverse items for post-processing
                    ( tagger { pov | channels = newChannels }
                    , { yield
                        | persist = True
                        , items = List.reverse ms
                        , updateFAM = updateOrKeepFAM updateFAM newChannels
                        , work = Just Worque.DiscordFetch
                      }
                    )

        Nothing ->
            -- Target Channel somehow gone; deleted?
            ( tagger pov
            , { yield
                | persist = True
                , updateFAM = calculateFAM pov.channels
                , work = Just Worque.DiscordFetch
              }
            )


handlePost : Discord -> PostOpts -> ( Discord, Yield )
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


postOrDiscard : (POV -> Discord) -> POV -> PostOpts -> ( Discord, Yield )
postOrDiscard stateTagger pov opts =
    let
        subscribed =
            Dict.get opts.channelId pov.channels
                |> Maybe.map (Channel.getFetchStatus >> FetchStatus.subscribed)
                |> Maybe.withDefault False
    in
    if subscribed then
        ( stateTagger pov, { yield | cmd = postChannelMessage pov.token opts } )

    else
        -- Discard if not subbed; Should not happen
        pure (stateTagger pov)


handlePosted : Discord -> PostSuccess -> ( Discord, Yield )
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


updatePovOnPostSuccess : (POV -> Discord) -> Channel.Id -> Posix -> POV -> ( Discord, Yield )
updatePovOnPostSuccess tagger cId posix pov =
    case Dict.get cId pov.channels of
        Just c ->
            let
                { fs, persist, updateFAM } =
                    FetchStatus.update (Spur posix) (Channel.getFetchStatus c)

                newChannels =
                    Dict.insert cId (Channel.setFetchStatus fs c) pov.channels
            in
            ( tagger { pov | channels = newChannels }
            , { yield | persist = persist, updateFAM = updateOrKeepFAM updateFAM newChannels }
            )

        Nothing ->
            -- Target Channel somehow gone; deleted?
            ( tagger pov, { yield | persist = True, updateFAM = calculateFAM pov.channels } )


handleChannelAPIError : Channel.Id -> HttpClient.Failure -> Discord -> ( Discord, Yield )
handleChannelAPIError cId ( httpError, req ) discord =
    case ( discord, httpError ) of
        ( Hydrated t pov, Unauthorized _ ) ->
            ( Expired t pov, { yield | persist = True } )

        ( Hydrated t pov, _ ) ->
            updatePovOnChannelAPIError (Hydrated t) cId httpError req pov

        ( Rehydrating t pov, Unauthorized _ ) ->
            ( Expired t pov, { yield | persist = True } )

        ( Rehydrating t pov, _ ) ->
            updatePovOnChannelAPIError (Rehydrating t) cId httpError req pov

        ( Expired _ _, Unauthorized _ ) ->
            pure discord

        ( Expired t pov, _ ) ->
            -- Late arrival?
            updatePovOnChannelAPIError (Expired t) cId httpError req pov

        ( Switching _ _, Unauthorized _ ) ->
            -- Hydrate should be going at the same time
            pure discord

        ( Switching newSession pov, _ ) ->
            -- Previous POV is still available
            updatePovOnChannelAPIError (Switching newSession) cId httpError req pov

        _ ->
            pure discord


updatePovOnChannelAPIError : (POV -> Discord) -> Channel.Id -> HttpClient.Error -> HttpClient.Req -> POV -> ( Discord, Yield )
updatePovOnChannelAPIError tagger cId httpError req pov =
    case Dict.get cId pov.channels of
        Just c ->
            if channelUnavailable httpError then
                let
                    newChannels =
                        Dict.remove cId pov.channels
                in
                ( tagger { pov | channels = newChannels }
                , { yield
                    | persist = True
                    , updateFAM = calculateFAM newChannels
                    , work = Just Worque.DiscordFetch
                  }
                )

            else
                -- Considered transient
                let
                    { fs, persist, updateFAM } =
                        FetchStatus.update Fail (Channel.getFetchStatus c)

                    newChannels =
                        Dict.insert cId (Channel.setFetchStatus fs c) pov.channels

                    work =
                        if wasFetchRequest cId req then
                            Just Worque.DiscordFetch

                        else
                            Nothing
                in
                ( tagger { pov | channels = newChannels }
                , { yield
                    | persist = persist
                    , updateFAM = updateOrKeepFAM updateFAM newChannels
                    , work = work
                  }
                )

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


wasFetchRequest : Channel.Id -> HttpClient.Req -> Bool
wasFetchRequest cId req =
    req.method == "GET" && String.endsWith (channelMessagesPath cId) req.url.path


handleGenericAPIError : Discord -> HttpClient.Failure -> ( Discord, Yield )
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


endpoint : String -> Maybe String -> Url
endpoint path queryMaybe =
    { protocol = Url.Https
    , host = "discordapp.com"
    , port_ = Nothing
    , path = "/api" ++ path
    , fragment = Nothing
    , query = queryMaybe
    }


identify : Token -> Cmd Msg
identify token =
    HttpClient.getWithAuth (endpoint "/users/@me" Nothing) (HttpClient.auth (fromToken token)) User.decoder
        |> HttpClient.try Identify GenericAPIError


hydrate : Token -> Cmd Msg
hydrate token =
    HttpClient.getWithAuth (endpoint "/users/@me/guilds" Nothing)
        (HttpClient.auth (fromToken token))
        (D.assocListFromList Guild.getId Guild.decoder)
        |> Task.andThen (hydrateChannels token)
        |> HttpClient.try identity GenericAPIError


hydrateChannels : Token -> Dict Guild.Id Guild -> Task HttpClient.Failure Msg
hydrateChannels token guilds =
    let
        -- TODO: we should also retrieve DMs, which are not tied to Guilds
        -- https://discordapp.com/developers/docs/resources/user#get-user-dms
        getGuildChannels guildId =
            HttpClient.getWithAuth (endpoint ("/guilds/" ++ Id.to guildId ++ "/channels") Nothing)
                (HttpClient.auth (fromToken token))
                (D.leakyList (Channel.decoder guilds))

        intoDict listOfChannelList =
            listOfChannelList
                |> List.concatMap (List.map (\channel -> ( Channel.getId channel, channel )))
                |> Dict.fromList
    in
    Dict.keys guilds
        |> List.map getGuildChannels
        |> Task.sequence
        |> Task.map (intoDict >> Hydrate guilds)


fetchChannelMessages : Token -> Channel -> Cmd Msg
fetchChannelMessages token channel =
    let
        cId =
            Channel.getId channel

        combiner messages posix =
            { channelId = cId, messages = messages, posix = posix }
    in
    Task.map2 combiner (fetchChannelMessagesTask token channel) Time.now
        |> HttpClient.try Fetched (ChannelAPIError cId)


fetchChannelMessagesTask : Token -> Channel -> Task HttpClient.Failure (List Message)
fetchChannelMessagesTask token channel =
    let
        query =
            case Channel.getLastMessageId channel of
                -- 100 is the maximum; <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
                Just lmid ->
                    Just ("limit=100&after=" ++ Id.to lmid)

                Nothing ->
                    -- Means never fetched
                    Just "limit=100"
    in
    -- Note that /messages API returns messages from latest to oldest
    HttpClient.getWithAuth (endpoint (channelMessagesPath (Channel.getId channel)) query)
        (HttpClient.auth (fromToken token))
        (D.leakyList Message.decoder)


channelMessagesPath : Channel.Id -> String
channelMessagesPath cId =
    "/channels/" ++ Id.to cId ++ "/messages"


postChannelMessage : Token -> PostOpts -> Cmd Msg
postChannelMessage token { channelId, message, file } =
    let
        postParts =
            List.filterMap identity
                [ Maybe.map (Http.stringPart "content") message
                , Maybe.map (Http.filePart "file") file
                ]

        postTask =
            HttpClient.postFormWithAuth (endpoint (channelMessagesPath channelId) Nothing)
                postParts
                (HttpClient.auth (fromToken token))
                (D.succeed ())

        combiner () posix =
            { channelId = channelId, posix = posix }
    in
    Task.map2 combiner postTask Time.now
        |> HttpClient.try Posted (ChannelAPIError channelId)



-- RUNTIME APIs


defaultIconUrl : Maybe Int -> String
defaultIconUrl sizeMaybe =
    makeDefaultIconUrl sizeMaybe ""


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
