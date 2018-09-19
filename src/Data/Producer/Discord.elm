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
import Json.Encode as E
import View.Parts exposing (disabled, disabledColor)
import Websocket exposing (Endpoint(..))


type alias Discord =
    { token : Token
    , userMaybe : Maybe User
    }


type alias User =
    { username : String
    , discriminator : String
    , email : String
    }


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
    D.field "tag" tagDecoder
        |> D.andThen
            (\_ ->
                D.map2 Discord
                    (D.field "token" tokenDecoder)
                    (D.field "user" (D.maybe userDecoder))
            )


tagDecoder : Decoder ()
tagDecoder =
    let
        readTagValue rawTag =
            case rawTag of
                "discord" ->
                    D.succeed ()

                _ ->
                    D.fail "Not a 'discord' tag."
    in
    D.string |> D.andThen readTagValue


tokenDecoder : Decoder Token
tokenDecoder =
    let
        genTagger rawTokenTag =
            case rawTokenTag of
                "New" ->
                    New

                "Ready" ->
                    Ready

                _ ->
                    -- Crash it!
                    genTagger rawTokenTag
    in
    D.field "tag" (D.map genTagger D.string)
        |> D.andThen (\tagger -> D.map tagger (D.field "val" D.string))


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
        , ( "token", encodeToken discord.token )
        , ( "user"
          , case discord.userMaybe of
                Just user ->
                    encodeUser user

                Nothing ->
                    E.null
          )
        ]


encodeToken : Token -> E.Value
encodeToken token =
    case token of
        New str ->
            E.object [ ( "tag", E.string "New" ), ( "val", E.string str ) ]

        Ready str ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]

        Authenticating str ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]

        Authenticated str ->
            E.object [ ( "tag", E.string "Ready" ), ( "val", E.string str ) ]


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


receive : Realtime.Handler Discord
receive discord message =
    case D.decodeString gatewayPayloadDecoder message of
        Ok (Hello _) ->
            case discord.token of
                Ready str ->
                    ( [ Data.Item.textOnly message ]
                    , { discord | token = Authenticating str }
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

        Ok (Dispatch (GatewayReady user)) ->
            case discord.token of
                Authenticating str ->
                    ( [ Data.Item.textOnly ("Authenticated as: " ++ user.username) ]
                    , { discord | token = Authenticated str, userMaybe = Just user }
                    , NoReply
                    )

                _ ->
                    -- Trap
                    receive discord message

        e ->
            -- Debug here
            ( [ Data.Item.textOnly message ], discord, NoReply )


type Payload
    = Hello Int
    | Dispatch Event
    | SomethingElse


type Event
    = GatewayReady User
    | Unknown


gatewayPayloadDecoder : Decoder Payload
gatewayPayloadDecoder =
    D.oneOf
        [ helloDecoder
        , dispatchDecoder

        -- , D.succeed SomethingElse
        ]


helloDecoder : Decoder Payload
helloDecoder =
    let
        checkOpCode op =
            if op == 10 then
                D.succeed ()

            else
                D.fail "Not hello."
    in
    D.field "op" (D.int |> D.andThen checkOpCode)
        |> D.andThen (\_ -> D.map Hello (D.at [ "d", "heartbeat_interval" ] D.int))


dispatchDecoder : Decoder Payload
dispatchDecoder =
    let
        checkOpCode op =
            if op == 0 then
                D.succeed ()

            else
                D.fail "Not dispatch."
    in
    D.field "op" (D.int |> D.andThen checkOpCode)
        |> D.andThen (\_ -> D.map Dispatch eventDecoder)


eventDecoder : Decoder Event
eventDecoder =
    let
        gatewayReadyTagger t =
            case t of
                "READY" ->
                    D.succeed GatewayReady

                _ ->
                    D.fail "Not GatewayReady."
    in
    D.oneOf
        [ D.field "t" (D.string |> D.andThen gatewayReadyTagger)
            |> D.andThen (\tagger -> D.map tagger (D.at [ "d", "user" ] userDecoder))
        , D.succeed Unknown
        ]


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


update : Msg -> Maybe Discord -> Maybe Discord
update msg discordMaybe =
    case ( msg, discordMaybe ) of
        ( TokenInput str, Just discord ) ->
            Just { discord | token = newTokenInput discord.token str }

        ( TokenInput str, Nothing ) ->
            Just (Discord (New str) Nothing)

        ( CommitToken, Just discord ) ->
            commitToken discord

        ( CommitToken, Nothing ) ->
            Nothing

        ( Timeout, Just discord ) ->
            handleTimeout discord

        ( Timeout, Nothing ) ->
            Nothing


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


commitToken : Discord -> Maybe Discord
commitToken discord =
    case discord.token of
        New "" ->
            Nothing

        New str ->
            Just { discord | token = Ready str }

        _ ->
            Just discord


handleTimeout : Discord -> Maybe Discord
handleTimeout discord =
    case discord.token of
        New _ ->
            Just discord

        Ready _ ->
            Just discord

        Authenticating _ ->
            Nothing

        Authenticated _ ->
            Just discord



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
