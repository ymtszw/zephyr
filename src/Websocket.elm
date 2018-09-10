module Websocket exposing (Endpoint(..), EventHandler, Key(..), State, engage, init, receive)

import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Ports
import WebSocketClient exposing (Response(..), close, makeConfig, makeState, openWithKey, process, send)


type alias State msg a b =
    { clientState : WebSocketClient.State msg
    , handlers : EventHandlers a b
    }


type alias EventHandlers a b =
    Dict String (EventHandler a b)


type Key
    = Key String


type Endpoint
    = Endpoint String


type alias EventHandler a b =
    { state : a
    , onMessage : a -> String -> ( List b, Reply, a )
    }


type Reply
    = Reply String
    | NoReply


init : List ( String, EventHandler a b ) -> State msg a b
init handlerList =
    State
        (makeState (makeConfig Ports.webSocketClientCmd))
        (Dict.fromList handlerList)


engage : State msg a b -> Key -> Endpoint -> EventHandler a b -> ( State msg a b, Cmd msg )
engage s (Key key) (Endpoint url) handler =
    url
        |> openWithKey WebSocketClient.PortVersion2 s.clientState key
        |> Tuple.mapBoth
            (\newClientState -> State newClientState (Dict.insert key handler s.handlers))
            handleOnEngageResponse


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


receive : State msg a b -> Value -> ( State msg a b, ( List b, Cmd msg ) )
receive s val =
    process s.clientState val
        |> Tuple.mapFirst (\newClientState -> { s | clientState = newClientState })
        |> handleOnReceiveResponse


handleOnReceiveResponse : ( State msg a b, Response msg ) -> ( State msg a b, ( List b, Cmd msg ) )
handleOnReceiveResponse ( s, res ) =
    case res of
        NoResponse ->
            ( s, ( [], Cmd.none ) )

        CmdResponse cmd ->
            ( s, ( [], cmd ) )

        ConnectedResponse _ ->
            ( s, ( [], Cmd.none ) )

        MessageReceivedResponse { key, message } ->
            applyHandlers s (Key key) message

        ClosedResponse _ ->
            -- Debug here
            ( s, ( [], Cmd.none ) )

        ErrorResponse err ->
            -- Debug here
            ( s, ( [], Cmd.none ) )


applyHandlers : State msg a b -> Key -> String -> ( State msg a b, ( List b, Cmd msg ) )
applyHandlers s (Key key) message =
    case Dict.get key s.handlers of
        Just handler ->
            withHandler s handler (Key key) message

        Nothing ->
            ( s, ( [], Cmd.none ) )


withHandler : State msg a b -> EventHandler a b -> Key -> String -> ( State msg a b, ( List b, Cmd msg ) )
withHandler s0 handler (Key key) message =
    let
        ( yields, reply, newState ) =
            handler.onMessage handler.state message

        newHandlers =
            Dict.insert key { handler | state = newState } s0.handlers

        ( cmd, s1 ) =
            handleReply { s0 | handlers = newHandlers } (Key key) reply
    in
    ( s1, ( yields, cmd ) )


handleReply : State msg a b -> Key -> Reply -> ( Cmd msg, State msg a b )
handleReply s (Key key) reply =
    case reply of
        Reply payload ->
            let
                ( newClientState, res ) =
                    send WebSocketClient.PortVersion2 s.clientState key payload
            in
            ( handleOnEngageResponse res, { s | clientState = newClientState } )

        NoReply ->
            ( Cmd.none, s )
