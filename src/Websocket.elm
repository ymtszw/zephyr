module Websocket exposing (Endpoint(..), Frame(..), Key(..), State, engage, init, receive, send)

import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Ports
import WebSocketClient exposing (Response(..))


type alias State msg =
    WebSocketClient.State msg


type Key
    = Key String


type Endpoint
    = Endpoint String


init : State msg
init =
    WebSocketClient.makeState (WebSocketClient.makeConfig Ports.webSocketClientCmd)


{-| Attempts to establish a new Websocket connection.

Key is required for distinguishing multiple connections to the same Endpoint.
You must `receive` subsequent server responses in order to actually use the connection.

-}
engage : Key -> Endpoint -> State msg -> ( State msg, Cmd msg )
engage (Key key) (Endpoint url) clientState =
    url
        |> WebSocketClient.openWithKey WebSocketClient.PortVersion2 clientState key
        |> issueOutgoingCmd


issueOutgoingCmd : ( State msg, Response msg ) -> ( State msg, Cmd msg )
issueOutgoingCmd ( clientState, res ) =
    Tuple.pair clientState <|
        case res of
            CmdResponse cmd ->
                cmd

            ErrorResponse err ->
                -- Debug here
                Cmd.none

            otherwise ->
                -- Debug here
                Cmd.none


type Frame msg
    = MessageFrame Key String
    | ControlFrame (Cmd msg)


{-| Receives arrived Websocket frame and sort them into Frame type.

Callers must post-process Frame and save new State.

-}
receive : State msg -> Value -> ( State msg, Frame msg )
receive clientState val =
    WebSocketClient.process clientState val
        |> Tuple.mapSecond convertResponse


convertResponse : Response msg -> Frame msg
convertResponse res =
    case res of
        NoResponse ->
            ControlFrame Cmd.none

        CmdResponse cmd ->
            ControlFrame cmd

        ConnectedResponse _ ->
            -- Debug here
            ControlFrame Cmd.none

        MessageReceivedResponse { key, message } ->
            MessageFrame (Key key) message

        ClosedResponse _ ->
            -- Debug here
            ControlFrame Cmd.none

        ErrorResponse err ->
            -- Debug here
            ControlFrame Cmd.none


{-| Send text frame through a Websocket connection specified by a Key.
-}
send : State msg -> Key -> String -> ( State msg, Cmd msg )
send clientState (Key key) payload =
    issueOutgoingCmd <|
        WebSocketClient.send WebSocketClient.PortVersion2 clientState key payload
