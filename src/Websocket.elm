module Websocket exposing (EventHandlers, MessageConverter, State, engage, handlers, init, receive)

import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Ports
import WebSocketClient exposing (Config, PortVersion(..), Response(..), makeConfig, makeState, openWithKey, process)


type alias State msg =
    WebSocketClient.State msg


type alias EventHandlers a =
    Dict Key (MessageConverter a)


type alias Key =
    String


type alias MessageConverter a =
    String -> List a


config : Config msg
config =
    makeConfig Ports.webSocketClientCmd


init : State msg
init =
    makeState config


handlers : List ( String, MessageConverter a ) -> EventHandlers a
handlers handlerList =
    Dict.fromList handlerList


engage : State msg -> Key -> String -> ( State msg, Cmd msg )
engage state key url =
    openWithKey PortVersion2 state key url
        |> Tuple.mapSecond handleOnEngageResponse


handleOnEngageResponse : Response msg -> Cmd msg
handleOnEngageResponse res =
    -- Basically it always return CmdResponse on open, otherwise ErrorResponse.
    -- If simulated, it can return ConnectedResponse
    case res of
        CmdResponse cmd ->
            cmd

        ErrorResponse err ->
            -- Debug here
            Cmd.none

        otherwise ->
            Cmd.none


receive : State msg -> EventHandlers a -> Value -> ( State msg, List a )
receive state eh val =
    process state val
        |> Tuple.mapSecond (handleOnReceiveResponse eh)


handleOnReceiveResponse : EventHandlers a -> Response msg -> List a
handleOnReceiveResponse eh res =
    case res of
        NoResponse ->
            []

        CmdResponse _ ->
            -- Command is for sending message from Elm application, thus won't happen on receive
            []

        ConnectedResponse _ ->
            -- Websocket APIs might return meaningful payload on connection.
            -- If so apply MessageConverter here
            []

        MessageReceivedResponse { key, message } ->
            applyHandlers eh key message

        ClosedResponse _ ->
            -- Debug here
            []

        ErrorResponse err ->
            -- Debug here
            []


applyHandlers : EventHandlers a -> Key -> String -> List a
applyHandlers eh key message =
    case Dict.get key eh of
        Just handler ->
            handler message

        Nothing ->
            []
