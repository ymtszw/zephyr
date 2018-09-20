module Data.Producer.Discord exposing (Discord, Msg(..), Token(..), configEl, decoder, encode, endpoint, receive, update)

{-| Realtime Producer for Discord.

Using Discord's realtime communication backend: "Gateway".

<https://discordapp.com/developers/docs/topics/gateway>

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
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import View.Parts exposing (disabled, disabledColor)
import Websocket exposing (Endpoint(..))


type alias Discord =
    { token : Token
    , interval : Maybe HeartbeatInterval
    , sessionMaybe : Maybe Session
    }


{-| Interval to send Opcode 1 Heartbeat in milliseconds.

Notified by server on conneciton establishment.

-}
type HeartbeatInterval
    = HI Int


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


{-| Custom type that represents token lifecycle.

  - When a user starts filling in token form, it becomes `New` token
  - When the above submitted, `New` changes to `Ready`
      - Form is locked until authentication attempted.
  - `Ready` value will be used on "identify" request over Websocket, becoming `Authenticating`
  - If it receives successful response, the value becomes `Authenticated`
      - Form is unlocked then.

-}
type Token
    = New String
    | Ready String
    | Authenticating String
    | Authenticated String


decoder : Decoder Discord
decoder =
    D.when (D.field "tag" D.string) ((==) "discord") <|
        D.map3 Discord
            (D.field "token" tokenDecoder)
            (D.field "interval" (D.succeed Nothing))
            (D.field "session" (D.maybe sessionDecoder))


tokenDecoder : Decoder Token
tokenDecoder =
    D.oneOf
        [ D.when (D.field "tag" D.string) ((==) "New") <|
            D.map New (D.field "val" D.string)
        , D.when (D.field "tag" D.string) ((==) "Ready") <|
            D.map Ready (D.field "val" D.string)
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
    E.object
        [ ( "tag", E.string "discord" )
        , ( "token", encodeToken discord )
        , ( "session"
          , case discord.sessionMaybe of
                Just session ->
                    encodeSession session

                Nothing ->
                    E.null
          )
        ]


encodeToken : Discord -> E.Value
encodeToken discord =
    -- Prioritize already authenticated token on persist
    case ( discord.token, discord.sessionMaybe ) of
        ( New str, Just session ) ->
            E.object [ ( "tag", E.string "New" ), ( "val", E.string session.token ) ]

        ( New str, Nothing ) ->
            E.object [ ( "tag", E.string "New" ), ( "val", E.string str ) ]

        ( Ready str, Just session ) ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string session.token ) ]

        ( Ready str, Nothing ) ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]

        ( Authenticating str, Just session ) ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string session.token ) ]

        ( Authenticating str, Nothing ) ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]

        ( Authenticated str, _ ) ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]


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


shouldLock : Token -> Bool
shouldLock token =
    case token of
        New _ ->
            False

        Ready _ ->
            True

        Authenticating _ ->
            True

        Authenticated _ ->
            False



-- PRODUCER INTERFACES


endpoint : Endpoint
endpoint =
    Endpoint "wss://gateway.discord.gg/?v=6&encoding=json"


type Payload
    = Hello HeartbeatInterval
    | Dispatch Event


type Event
    = GatewayReady User SequenceNumber SessionId


receive : Realtime.Handler Discord
receive discord message =
    case D.decodeString gatewayPayloadDecoder message of
        Ok (Hello hi) ->
            case discord.token of
                Ready str ->
                    ( [ Data.Item.textOnly message ]
                    , { discord | token = Authenticating str, interval = Just hi }
                    , ReplyWithTimeout (identifyPayload str) 5000
                    )

                Authenticated str ->
                    -- Just re-"identify"; better try "resume" first then "identify" on failure
                    ( [ Data.Item.textOnly message ]
                    , { discord | token = Authenticating str }
                    , ReplyWithTimeout (identifyPayload str) 5000
                    )

                _ ->
                    ( [ Data.Item.textOnly message ], discord, NoReply )

        Ok (Dispatch (GatewayReady user seq id)) ->
            case discord.token of
                Authenticating str ->
                    ( [ Data.Item.textOnly ("Authenticated as: " ++ user.username) ]
                    , { discord
                        | token = Authenticated str
                        , sessionMaybe = Just (Session user seq id str)
                      }
                    , NoReply
                    )

                _ ->
                    -- Trap
                    receive discord message

        e ->
            -- Debug here
            ( [ Data.Item.textOnly message ], discord, NoReply )


gatewayPayloadDecoder : Decoder Payload
gatewayPayloadDecoder =
    D.oneOf
        [ helloDecoder
        , dispatchDecoder
        ]


helloDecoder : Decoder Payload
helloDecoder =
    D.when (D.field "op" D.int) ((==) 10) <|
        D.map (Hello << HI) (D.at [ "d", "heartbeat_interval" ] D.int)


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
                            [ ( "$os", E.string "linux" )
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
            ( Just { discord | token = newTokenInput discord.token str }, NoReply )

        ( TokenInput str, Nothing ) ->
            ( Just (Discord (New str) Nothing Nothing), NoReply )

        ( CommitToken, Just discord ) ->
            commitToken discord

        ( CommitToken, Nothing ) ->
            ( Nothing, NoReply )

        ( Timeout, Just discord ) ->
            handleTimeout discord

        ( Timeout, Nothing ) ->
            ( Nothing, NoReply )


newTokenInput : Token -> String -> Token
newTokenInput token newToken =
    case token of
        Ready _ ->
            token

        Authenticating _ ->
            -- Currently authenticating token cannot be overwritten
            token

        _ ->
            New newToken


commitToken : Discord -> ( Maybe Discord, Reply )
commitToken discord =
    case ( discord.token, discord.sessionMaybe ) of
        ( New "", Nothing ) ->
            ( Nothing, NoReply )

        ( New "", Just _ ) ->
            ( Nothing, Disengage )

        ( New str, Just _ ) ->
            -- Attempt to replace identify of current connection.
            -- TODO need to check if this is even possible; if not, reconnect must be issued
            ( Just { discord | token = Ready str }, Reply (identifyPayload str) )

        ( New str, Nothing ) ->
            ( Just { discord | token = Ready str }, Engage )

        _ ->
            -- Should not basically happen; though it could in extreme rare situations
            ( Just discord, NoReply )


handleTimeout : Discord -> ( Maybe Discord, Reply )
handleTimeout discord =
    case discord.token of
        New _ ->
            ( Just discord, NoReply )

        Ready _ ->
            ( Just discord, NoReply )

        Authenticating _ ->
            ( Nothing, Disengage )

        Authenticated _ ->
            ( Just discord, NoReply )



-- CONFIG VIEW


configEl : Maybe Discord -> Element Msg
configEl discordMaybe =
    case discordMaybe of
        Just { token } ->
            tokenFormEl token

        Nothing ->
            tokenFormEl (New "")


tokenFormEl : Token -> Element Msg
tokenFormEl token =
    El.column
        [ El.width El.fill, El.spacing 5 ]
        [ Element.Input.text
            (disabled (shouldLock token)
                [ El.width El.fill
                , El.padding 5
                , BG.color oneDark.note
                , BD.width 0
                ]
            )
            { onChange = TokenInput
            , text = tokenText token
            , placeholder = Nothing
            , label = tokenLabelEl
            }
        , Element.Input.button
            ([ El.alignRight
             , El.width (El.fill |> El.maximum 150)
             , El.padding 10
             , BD.rounded 5
             ]
                |> disabled (shouldLock token)
                |> disabledColor (shouldLock token)
            )
            { onPress =
                case token of
                    New _ ->
                        Just CommitToken

                    _ ->
                        Nothing
            , label =
                El.text <|
                    case token of
                        New _ ->
                            "Submit"

                        Ready _ ->
                            "Waiting..."

                        Authenticating _ ->
                            "Authenticating..."

                        Authenticated _ ->
                            "Submit"
            }
        ]


tokenText : Token -> String
tokenText token =
    case token of
        New str ->
            str

        Ready str ->
            str

        Authenticating str ->
            str

        Authenticated str ->
            str


tokenLabelEl : Element.Input.Label msg
tokenLabelEl =
    Element.Input.labelAbove [] <|
        El.column [ El.spacing 5 ]
            [ El.el [] (El.text "Token")
            , El.el [ Font.color oneDark.note, Font.size 14 ] <|
                El.text "Some shady works required to acquire Discord personal access token. Do not talk about it."
            ]
