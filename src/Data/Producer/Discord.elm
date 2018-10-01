module Data.Producer.Discord exposing (Discord(..), Msg(..), configEl, decoder, encode, reload, update)

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.

-}

import Data.ColorTheme exposing (oneDark)
import Data.Item as Item exposing (Item)
import Data.Producer.Base as Producer exposing (save)
import Data.Producer.Realtime as Realtime exposing (Reply(..))
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Extra exposing (ite)
import Http
import HttpExtra as Http
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Task exposing (Task)
import Time exposing (Posix)
import Url exposing (Url)
import View.Parts exposing (disabled, disabledColor)
import Websocket exposing (Endpoint(..))



-- TYPES


{-| Discord state itself is a custom type that represents authentication status.

  - When a user starts filling in token form for the first time, it becomes `TokenGiven` state
  - When the above submitted, changes to `TokenReady`
      - Form is locked while authentication attempted.
  - On successful response from Current User API, it becomes `Identified` with NewSession data.
  - After that, available Guild and Channel lists are retrieved from Discord API.
    When they are all ready, it becomes `Hydrated`.
      - Form is unlocked then.
      - Once fully-hydrated state will be saved to IndexedDB.
  - Upon application reload, it starts with `Revisit` status,
    then become `Hydrated` again if the token successfully confirmed.
      - If not, it becomes `Expired` (it could also mean the token is revoked by the server)
  - When token is changed to one for another user, it stops at `Switching` state,
    requesting user confirmation, then move to `Identified`, discarding old Config.

-}
type Discord
    = TokenGiven String
    | TokenReady String
    | Identified NewSession
    | Hydrated String PoV
    | Revisit PoV
    | Expired String PoV
    | Switching NewSession PoV


{-| Current user's point of view.

In Discord, it contains:

  - Working token
  - Login User info
  - ID Dict of subscribing Guilds. May be updated periodically.
  - ID Dict of Channels in subscribing Guilds. Maybe updated periodically

-}
type alias PoV =
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
    , email : String
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
    , lastMessageId : Maybe MessageId

    -- Zephyr-only fields below
    , lastFetchTime : Maybe Posix
    , lastYieldTime : Maybe Posix
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
    = Emoji { id : String, ext : String }
    | GuildIcon { guildId : String, hash : String }
    | UserAvatar { userId : String, hash : String }



-- DECODER


decoder : Decoder Discord
decoder =
    D.oneOf
        -- Saved state always starts with at best TokenReady state
        [ D.when (D.field "tag" D.string) ((==) "discordRevisit") <|
            D.map Revisit (D.field "pov" povDecoder)
        , D.when (D.field "tag" D.string) ((==) "discordTokenReady") <|
            D.map TokenReady (D.field "token" D.string)
        , D.when (D.field "tag" D.string) ((==) "discordTokenGiven") <|
            D.map TokenGiven (D.field "token" D.string)
        ]


povDecoder : Decoder PoV
povDecoder =
    D.map4 PoV
        (D.field "token" D.string)
        (D.field "user" userDecoder)
        (D.field "guilds" (D.dict guildDecoder))
        (D.field "channels" (D.dict channelDecoder))


userDecoder : Decoder User
userDecoder =
    D.map4 User
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "discriminator" D.string)
        (D.field "email" D.string)


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


channelDecoder : Decoder Channel
channelDecoder =
    let
        flatten aMaybeMaybe =
            case aMaybeMaybe of
                Just (Just a) ->
                    Just a

                _ ->
                    Nothing
    in
    D.map6 Channel
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "type" channelTypeDecoder)
        (D.map flatten (D.maybe (D.field "last_message_id" (D.maybe (D.map MessageId D.string)))))
        (D.map flatten (D.maybe (D.field "lastFetchTime" (D.maybe (D.map Time.millisToPosix D.int)))))
        (D.map flatten (D.maybe (D.field "lastYieldTime" (D.maybe (D.map Time.millisToPosix D.int)))))


channelTypeDecoder : Decoder ChannelType
channelTypeDecoder =
    D.string
        |> D.andThen
            (\string ->
                case string of
                    "GUILD_TEXT" ->
                        D.succeed GuildText

                    "DM" ->
                        D.succeed DM

                    "GROUP_DM" ->
                        D.succeed GroupDM

                    _ ->
                        D.fail "Invalid ChannelType"
            )



-- ENCODER


encode : Discord -> E.Value
encode discord =
    case discord of
        TokenGiven token ->
            E.object
                [ ( "tag", E.string "discordTokenGiven" )
                , ( "token", E.string token )
                ]

        TokenReady token ->
            E.object
                [ ( "tag", E.string "discordTokenReady" )
                , ( "token", E.string token )
                ]

        Identified session ->
            -- New interval should be retrieved on next connection, not persisting old one
            -- Step back to TokenReady state for retry
            E.object
                [ ( "tag", E.string "discordTokenReady" )
                , ( "token", E.string session.token )
                ]

        Hydrated _ pov ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "pov", encodePov pov )
                ]

        Switching _ pov ->
            -- Not persisting yet-authenticated new token
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "pov", encodePov pov )
                ]

        Revisit pov ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "pov", encodePov pov )
                ]

        Expired _ pov ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "pov", encodePov pov )
                ]


encodePov : PoV -> E.Value
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
        , ( "email", E.string user.email )
        ]


encodeGuildDict : Dict String Guild -> E.Value
encodeGuildDict guilds =
    guilds |> Dict.map (\_ v -> encodeGuild v) |> Dict.toList |> E.object


encodeGuild : Guild -> E.Value
encodeGuild guild =
    let
        encodeGuildIcon icon =
            case icon of
                GuildIcon { hash } ->
                    E.string hash

                _ ->
                    -- Trap
                    encodeGuildIcon icon
    in
    E.object
        [ ( "id", E.string guild.id )
        , ( "name", E.string guild.name )
        , ( "icon", E.maybe encodeGuildIcon guild.icon )
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
        , ( "last_message_id", E.maybe (unwrapMessageId >> E.string) channel.lastMessageId ) -- Match field name with Discord's API
        , ( "lastFetchTime", E.maybe (Time.posixToMillis >> E.int) channel.lastFetchTime )
        , ( "lastYieldTime", E.maybe (Time.posixToMillis >> E.int) channel.lastYieldTime )
        ]


encodeChannelType : ChannelType -> E.Value
encodeChannelType type_ =
    case type_ of
        GuildText ->
            E.string "GUILD_TEXT"

        DM ->
            E.string "DM"

        GroupDM ->
            E.string "GROUP_DM"



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
    | APIError Http.Error


update : Producer.Update Discord Msg
update msg discordMaybe =
    case ( msg, discordMaybe ) of
        ( TokenInput str, Just discord ) ->
            save (Just (tokenInput discord str))

        ( TokenInput str, Nothing ) ->
            save (Just (TokenGiven str))

        ( CommitToken, Just discord ) ->
            commitToken discord

        ( Identify user, Just discord ) ->
            handleIdentify discord user

        ( Hydrate guilds channels, Just discord ) ->
            handleHydrate discord guilds channels

        ( APIError e, Just discord ) ->
            handleAPIError discord e

        ( _, Nothing ) ->
            -- Msg other than TokenInput should not arrive when Discord state is missing.
            save Nothing


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


commitToken : Discord -> Producer.Yield Discord Msg
commitToken discord =
    case discord of
        TokenGiven "" ->
            save Nothing

        TokenGiven token ->
            ( [], Just (TokenReady token), identify token )

        Hydrated "" _ ->
            -- TODO Insert confirmation phase later
            save Nothing

        Hydrated newToken pov ->
            ( [], Just discord, identify newToken )

        Expired "" _ ->
            -- TODO Insert confirmation phase later
            save Nothing

        Expired newToken pov ->
            ( [], Just discord, identify newToken )

        _ ->
            -- Otherwise token input is locked; this should not happen
            commitToken discord


handleIdentify : Discord -> User -> Producer.Yield Discord Msg
handleIdentify discord user =
    case discord of
        TokenReady token ->
            ( [ Item.textOnly ("User: " ++ user.username) ]
            , Just (Identified (NewSession token user))
            , hydrate token
            )

        Hydrated token pov ->
            detectUserSwitch token pov user

        Expired token pov ->
            detectUserSwitch token pov user

        Revisit pov ->
            save (Just (Hydrated pov.token { pov | user = user }))

        Switching _ pov ->
            -- Retry Identify with previous token after error on Switching phase
            detectUserSwitch pov.token pov user

        _ ->
            -- Otherwise Identify should not arrive
            handleIdentify discord user


detectUserSwitch : String -> PoV -> User -> Producer.Yield Discord Msg
detectUserSwitch token pov user =
    if user.id == pov.user.id then
        save (Just (Hydrated token { pov | token = token, user = user }))

    else
        -- TODO Insert confirmation phase later
        ( [ Item.textOnly ("New User: " ++ user.username) ]
        , Just (Switching (NewSession token user) pov)
        , hydrate token
        )


handleHydrate : Discord -> Dict String Guild -> Dict String Channel -> Producer.Yield Discord Msg
handleHydrate discord guilds channels =
    case discord of
        Identified { token, user } ->
            ( printGuilds guilds, Just (Hydrated token (PoV token user guilds channels)), Cmd.none )

        Switching { token, user } _ ->
            ( printGuilds guilds, Just (Hydrated token (PoV token user guilds channels)), Cmd.none )

        _ ->
            -- Otherwise Hydrate should not arrive
            handleHydrate discord guilds channels


printGuilds : Dict String Guild -> List Item
printGuilds guilds =
    Dict.foldl (\_ guild acc -> Item.textOnly ("Watching: " ++ guild.name) :: acc) [] guilds


handleAPIError : Discord -> Http.Error -> Producer.Yield Discord Msg
handleAPIError discord error =
    -- Debug here; mostly, unexpected API errors indicate auth error
    case discord of
        TokenGiven _ ->
            -- Late arrival of API response started in already discarded Discord state? Ignore.
            save (Just discord)

        TokenReady _ ->
            save Nothing

        Identified _ ->
            -- If successfully Identified, basically Hydrate should not fail. Fall back to token input.
            save Nothing

        Hydrated _ pov ->
            save (Just (Hydrated pov.token pov))

        Revisit pov ->
            save (Just (Expired pov.token pov))

        Expired token pov ->
            save (Just discord)

        Switching _ pov ->
            -- Similar to Identified branch. Retry Identify with previous token
            ( [], Just discord, identify pov.token )



-- REST API CLIENTS


apiPath : String -> Url
apiPath path =
    { protocol = Url.Https
    , host = "discordapp.com"
    , port_ = Nothing
    , path = "/api" ++ path
    , fragment = Nothing
    , query = Nothing
    }


identify : String -> Cmd Msg
identify token =
    Http.getWithAuth (apiPath "/users/@me") (Http.auth token) userDecoder
        |> Http.try Identify APIError


hydrate : String -> Cmd Msg
hydrate token =
    Http.getWithAuth (apiPath "/users/@me/guilds") (Http.auth token) decodeGuildArrayIntoDict
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
            Http.getWithAuth (apiPath ("/guilds/" ++ guildId ++ "/channels"))
                (Http.auth token)
                (D.leakyList channelDecoder)

        intoDict listOfChannelList =
            listOfChannelList
                |> List.concatMap (List.map (\channel -> ( channel.id, channel )))
                |> Dict.fromList
    in
    Dict.keys guilds
        |> List.map getGuildChannels
        |> Task.sequence
        |> Task.map (intoDict >> Hydrate guilds)



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
    El.column
        [ El.width El.fill, El.spacing 5 ]
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
        , Element.Input.button
            ([ El.alignRight
             , El.width (El.fill |> El.maximum 150)
             , El.padding 10
             , BD.rounded 5
             ]
                |> disabled (shouldLockButton discord)
                |> disabledColor (shouldLockButton discord)
            )
            { onPress = ite (shouldLockButton discord) Nothing (Just CommitToken)
            , label = El.text (tokenInputButtonLabel discord)
            }
        ]


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
            , El.el [ Font.color oneDark.note, Font.size 14 ] <|
                El.text "Some shady works required to acquire Discord personal access token. Do not talk about it."
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

        Switching _ _ ->
            "Switching Identity"

        Revisit _ ->
            "Reloading..."

        Expired _ _ ->
            "Change Token"
