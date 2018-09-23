module Data.Producer.Discord exposing (Discord(..), Msg(..), configEl, decoder, encode, endpoint, receive, update)

{-| Polling Producer for Discord.

Using Discord's RESTful APIs to retrieve Items.

<https://discordapp.com/developers/docs/intro>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.

-}

import Data.ColorTheme exposing (oneDark)
import Data.Item exposing (Item)
import Data.Producer.Realtime as Realtime exposing (Reply(..))
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Extra exposing (ite)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import View.Parts exposing (disabled, disabledColor)
import Websocket exposing (Endpoint(..))


{-| Discord state itself is a custom type that represents authentication status.

  - When a user starts filling in token form, it becomes `TokenGiven` state
  - When the above submitted, changes to `TokenReady`
      - Form is locked while authentication attempted.
  - On successful response from Current User API, it becomes `Identified`
      - Form is unlocked then.
  - Session (token, login user info) and Config (watching servers and channels)
    will be saved to IndexedDB.
  - Upon application reload, it starts with `Revisit` status,
    then become `Identified` if the token successfully confirmed again.
      - If not, it becomes `Expired`
  - When token is changed to one for another user, it stops at `Switching` state,
    requesting user confirmation, then move to `Identified`, discarding old Config.

-}
type Discord
    = TokenGiven String
    | TokenReady String
    | Identified String Session Config
    | Revisit Session Config
    | Expired String Session Config
    | Switching Session Session Config


{-| Interval to send Opcode 1 Heartbeat in milliseconds.

Notified by server on conneciton establishment.

-}
type HeartbeatInterval
    = HI Float


{-| Session information of the connection.

Notified by server on authentication success by Gateway Ready event.

-}
type alias Session =
    { user : User
    , s : SequenceNumber
    , id : SessionId
    , token : String -- Hold currently used token in order to "reset" dirty form
    }


type alias User =
    { username : String
    , discriminator : String
    , email : String
    }


type SequenceNumber
    = S Int


type SessionId
    = SessionId String


{-| Ratch to limit there is only one timer exists;
i.e. authentication checker, or heartbeat. Not both.
-}
type alias TimeoutRatch =
    Bool


decoder : Decoder Discord
decoder =
    D.oneOf
        -- Saved state always starts with at best TokenReady state
        [ D.when (D.field "tag" D.string) ((==) "discordRevisit") <|
            D.map Revisit (D.field "session" sessionDecoder)
        , D.when (D.field "tag" D.string) ((==) "discordTokenReady") <|
            D.map TokenReady (D.field "token" D.string)
        , D.when (D.field "tag" D.string) ((==) "discordTokenGiven") <|
            D.map TokenGiven (D.field "token" D.string)
        ]


sessionDecoder : Decoder Session
sessionDecoder =
    D.map4 Session
        (D.field "user" userDecoder)
        (D.field "s" (D.map S D.int))
        (D.field "id" (D.map SessionId D.string))
        (D.field "token" D.string)


userDecoder : Decoder User
userDecoder =
    D.map3 User
        (D.field "username" D.string)
        (D.field "discriminator" D.string)
        (D.field "email" D.string)


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

        Connected token _ _ ->
            -- New interval should be retrieved on next connection, not persisting old one
            -- Step back to TokenReady state for retry
            E.object
                [ ( "tag", E.string "discordTokenReady" )
                , ( "token", E.string token )
                ]

        Identified _ _ session _ ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "session", encodeSession session )
                ]

        Switching _ _ session _ ->
            -- Not persisting yet-authenticated new token
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "session", encodeSession session )
                ]

        Revisit session ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "session", encodeSession session )
                ]

        Reconnected _ session _ ->
            E.object
                [ ( "tag", E.string "discordRevisit" )
                , ( "session", encodeSession session )
                ]


encodeSession : Session -> E.Value
encodeSession session =
    let
        ( S seq, SessionId id ) =
            ( session.s, session.id )
    in
    E.object
        [ ( "user", encodeUser session.user )
        , ( "s", E.int seq )
        , ( "id", E.string id )
        , ( "token", E.string session.token )
        ]


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "username", E.string user.username )
        , ( "discriminator", E.string user.discriminator )
        , ( "email", E.string user.email )
        ]


shouldLock : Discord -> Bool
shouldLock discord =
    case discord of
        TokenGiven _ ->
            False

        TokenReady _ ->
            True

        Connected _ _ _ ->
            True

        Identified _ _ _ _ ->
            False

        Switching _ _ _ _ ->
            True

        Revisit _ ->
            True

        Reconnected _ _ _ ->
            -- XXX There must be a "force reset" button, since token may be revoked by server
            True



-- PRODUCER INTERFACES


endpoint : Endpoint
endpoint =
    Endpoint "wss://gateway.discord.gg/?v=6&encoding=json"


type Payload
    = HeartbeatAck
    | HeartbeatReq
    | Hello HeartbeatInterval
    | Dispatch Event


type Event
    = GatewayReady User SequenceNumber SessionId


identifyTimeoutMs : Float
identifyTimeoutMs =
    10000


receive : Realtime.Handler Discord
receive discord message =
    case D.decodeString gatewayPayloadDecoder message of
        Ok HeartbeatAck ->
            -- Here we should set timeout for next heartbeat basically, only exception is on reconnect
            case discord of
                Connected token (HI hi) ratch ->
                    ( [], Connected token (HI hi) True, ite ratch NoReply (OnlyTimeout hi) )

                Identified token (HI hi) session ratch ->
                    ( [], Identified token (HI hi) session True, ite ratch NoReply (OnlyTimeout hi) )

                Switching token (HI hi) session ratch ->
                    ( [], Switching token (HI hi) session True, ite ratch NoReply (OnlyTimeout hi) )

                Reconnected hi session ratch ->
                    -- Server responding to heartbeat but not to identify. Cloud be outage. Keep trying identify.
                    ( []
                    , Reconnected hi session True
                    , ite ratch
                        (Reply (identifyPayload session.token))
                        (ReplyWithTimeout (identifyPayload session.token) identifyTimeoutMs)
                    )

                _ ->
                    -- Heartbeat arriving but Discord state does not have HeartbeatInterval!? Crash.
                    receive discord message

        Ok HeartbeatReq ->
            -- In this case we should immediately send heartbeat then wait for ack.
            case discord of
                Connected _ _ _ ->
                    -- Should not happen; server do not request heartbeat on initial connection
                    ( [], discord, NoReply )

                Identified _ _ session _ ->
                    ( [], discord, Reply (heartbeatPayload session.s) )

                Switching _ _ session _ ->
                    ( [], discord, Reply (heartbeatPayload session.s) )

                Reconnected hi session ratch ->
                    -- Server responding to heartbeat but not to identify. Cloud be outage. Keep trying identify.
                    ( []
                    , Reconnected hi session True
                    , ite ratch
                        (Reply (identifyPayload session.token))
                        (ReplyWithTimeout (identifyPayload session.token) identifyTimeoutMs)
                    )

                _ ->
                    -- Heartbeat arriving but Discord state does not have HeartbeatInterval!? Crash.
                    receive discord message

        Ok (Hello hi) ->
            -- Attempt identify and activate "check back later" timeout to detect authentication failure.
            -- This should be done by handling connection closure by server (next version of elm-websocket-client).
            case discord of
                TokenReady token ->
                    ( []
                    , Connected token hi True
                    , ReplyWithTimeout (identifyPayload token) identifyTimeoutMs
                    )

                Revisit session ->
                    -- Just re-"identify" with new token. TODO use resume, then identify if rejected
                    ( []
                    , Reconnected hi session True
                    , ReplyWithTimeout (identifyPayload session.token) identifyTimeoutMs
                    )

                Identified _ _ session ratch ->
                    -- Somehow lost connection and reconnected, could be outage; TODO use resume, then identify if rejected
                    ( []
                    , Reconnected hi session True
                    , ite ratch
                        (Reply (identifyPayload session.token))
                        (ReplyWithTimeout (identifyPayload session.token) identifyTimeoutMs)
                    )

                _ ->
                    ( [], discord, NoReply )

        Ok (Dispatch (GatewayReady user seq id)) ->
            case discord of
                Connected token (HI hi) ratch ->
                    ( [ Data.Item.textOnly ("Authenticated as: " ++ user.username) ]
                    , Identified token (HI hi) (Session user seq id token) True
                    , ite ratch NoReply (OnlyTimeout hi)
                    )

                Reconnected (HI hi) oldSession ratch ->
                    ( [ Data.Item.textOnly ("Authenticated as: " ++ user.username) ]
                    , Identified oldSession.token (HI hi) (Session user seq id oldSession.token) True
                    , ite ratch NoReply (OnlyTimeout hi)
                    )

                _ ->
                    -- Trap; if we currently trying to authenticate, there must be a HeartbeatInterval
                    receive discord message

        e ->
            -- Debug here
            -- The only default behavior is revive heartbeat timer if somehow dead (ratch == False).
            case discord of
                Connected token (HI hi) False ->
                    ( [ Data.Item.textOnly message ], Connected token (HI hi) True, OnlyTimeout hi )

                Identified token (HI hi) session False ->
                    ( [ Data.Item.textOnly message ], Identified token (HI hi) session True, ReplyWithTimeout (heartbeatPayload session.s) hi )

                Switching token (HI hi) session False ->
                    ( [ Data.Item.textOnly message ], Switching token (HI hi) session True, ReplyWithTimeout (heartbeatPayload session.s) hi )

                Reconnected (HI hi) session False ->
                    ( [ Data.Item.textOnly message ], Reconnected (HI hi) session True, ReplyWithTimeout (identifyPayload session.token) hi )

                _ ->
                    ( [ Data.Item.textOnly message ], discord, NoReply )


gatewayPayloadDecoder : Decoder Payload
gatewayPayloadDecoder =
    D.oneOf
        -- Should be ordered by frequency
        [ heartbeatAckDecoder
        , dispatchDecoder
        , helloDecoder
        , heartbeatReqDecoder
        ]


heartbeatAckDecoder : Decoder Payload
heartbeatAckDecoder =
    D.when (D.field "op" D.int) ((==) 11) (D.succeed HeartbeatAck)


heartbeatReqDecoder : Decoder Payload
heartbeatReqDecoder =
    D.when (D.field "op" D.int) ((==) 1) (D.succeed HeartbeatReq)


helloDecoder : Decoder Payload
helloDecoder =
    D.when (D.field "op" D.int) ((==) 10) <|
        D.map (Hello << HI) (D.at [ "d", "heartbeat_interval" ] D.float)


dispatchDecoder : Decoder Payload
dispatchDecoder =
    D.when (D.field "op" D.int) ((==) 0) <|
        D.map Dispatch eventDecoder


eventDecoder : Decoder Event
eventDecoder =
    D.oneOf
        [ D.when (D.field "t" D.string) ((==) "READY") gatewayReadyDecoder
        ]


gatewayReadyDecoder : Decoder Event
gatewayReadyDecoder =
    D.map3 GatewayReady
        (D.at [ "d", "user" ] userDecoder)
        (D.field "s" (D.map S D.int))
        (D.at [ "d", "session_id" ] (D.map SessionId D.string))


identifyPayload : String -> String
identifyPayload tokenStr =
    E.encode 0 <|
        E.object
            [ ( "op", E.int 2 )
            , ( "d"
              , E.object
                    [ ( "token", E.string tokenStr )
                    , ( "properties"
                      , E.object
                            [ ( "$os", E.string "other" )
                            , ( "$browser", E.string "disco" )
                            , ( "$device", E.string "disco" )
                            ]
                      )
                    , ( "shard", E.list E.int [ 1, 10 ] )
                    , ( "presence"
                      , E.object
                            [ ( "since", E.null )
                            , ( "game", E.null )
                            , ( "status", E.string "invisible" )
                            , ( "afk", E.bool False )
                            ]
                      )
                    ]
              )
            ]



-- UPDATE


type Msg
    = TokenInput String
    | CommitToken
    | Timeout


update : Msg -> Maybe Discord -> ( Maybe Discord, Reply )
update msg discordMaybe =
    case ( msg, discordMaybe ) of
        ( TokenInput str, Just discord ) ->
            ( Just (tokenInput discord str), NoReply )

        ( TokenInput str, Nothing ) ->
            ( Just (TokenGiven str), NoReply )

        ( CommitToken, Just discord ) ->
            commitToken discord

        ( CommitToken, Nothing ) ->
            ( Nothing, NoReply )

        ( Timeout, Just discord ) ->
            handleTimeout discord

        ( Timeout, Nothing ) ->
            ( Nothing, NoReply )


tokenInput : Discord -> String -> Discord
tokenInput discord newToken =
    case discord of
        TokenGiven _ ->
            TokenGiven newToken

        Identified _ hi session ratch ->
            Identified newToken hi session ratch

        _ ->
            -- Committed/just loaded/authenticating token cannot be overwritten until auth attempt resolved
            discord


commitToken : Discord -> ( Maybe Discord, Reply )
commitToken discord =
    case discord of
        TokenGiven "" ->
            ( Nothing, NoReply )

        Identified "" _ _ _ ->
            ( Nothing, Disengage )

        Identified newToken _ _ ratch ->
            -- Attempt to replace identity of current connection.
            -- TODO need to check if this is even possible; if not, reconnect must be issued first
            ( Just discord
            , ite ratch
                (Reply (identifyPayload newToken))
                (ReplyWithTimeout (identifyPayload newToken) identifyTimeoutMs)
            )

        TokenGiven str ->
            ( Just (TokenReady str), Engage )

        _ ->
            -- Should not basically happen; though it could in extreme rare situations
            ( Just discord, NoReply )


handleTimeout : Discord -> ( Maybe Discord, Reply )
handleTimeout discord =
    -- Note that we must wait for "heartbeat ack" before registering timer for next heartbeat.
    -- If ack does not arrive in timely manner, we must close connection and reconnect (NYI).
    case discord of
        Connected _ _ _ ->
            -- First ever authentication attempt not succeeded in time; definitely failure
            ( Nothing, Disengage )

        Identified token hi session _ ->
            -- "Normal" case, send heartbeat; kill ratch so that next timeout can be set on ack.
            -- If ack does not arrive, connection may die anytime thereafter.
            -- Can be revived with any request from server.
            -- Otherwise it lives until elm-websocket-client decides to kill the connection.
            -- If reconnected later, re-auth is attempted by receive function.
            ( Just (Identified token hi session False), Reply (heartbeatPayload session.s) )

        Switching _ (HI hi) oldSession _ ->
            -- New authentication attempt failed, re-identify with old one
            -- TODO Need to check its possibility, as in commitToken.
            -- XXX Possibly, heartbeat timeout may arrive at this state before server returns Gateway Ready.
            -- Ideally we should be able to distinguish auth failure (server close) and heartbeat timeout,
            -- which should be possible in the next version of elm-websocket-client
            ( Just (Reconnected (HI hi) oldSession True), ReplyWithTimeout (identifyPayload oldSession.token) hi )

        Reconnected (HI hi) session _ ->
            -- Keep sending heartbeat if "identify" not succeeding when it should. Possibly outage.
            -- "Identify" will be retried on ack.
            -- XXX Token may be revoked by the server. We need "force reset" button
            ( Just (Reconnected (HI hi) session False), Reply (heartbeatPayload session.s) )

        _ ->
            -- Timeout when there is no connection!? Crash
            handleTimeout discord


heartbeatPayload : SequenceNumber -> String
heartbeatPayload (S seq) =
    E.encode 0 <|
        E.object [ ( "op", E.int 1 ), ( "d", E.int seq ) ]



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
            (disabled (shouldLock discord)
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


shouldLockButton : Discord -> Bool
shouldLockButton discord =
    case discord of
        TokenGiven _ ->
            False

        Identified newToken _ session _ ->
            newToken == session.token

        _ ->
            True


tokenText : Discord -> String
tokenText discord =
    case discord of
        TokenGiven string ->
            string

        TokenReady string ->
            string

        Connected string _ _ ->
            string

        Identified string _ _ _ ->
            string

        Switching string _ _ _ ->
            string

        Revisit session ->
            session.token

        Reconnected _ session _ ->
            session.token


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
            "Submit"

        TokenReady _ ->
            "Waiting..."

        Connected _ _ _ ->
            "Authenticating..."

        Identified _ _ _ _ ->
            "Submit"

        Switching _ _ _ _ ->
            "Switching user..."

        Revisit _ ->
            "Reconnecting..."

        Reconnected _ _ _ ->
            "Re-authenticating..."
