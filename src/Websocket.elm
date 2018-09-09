module Websocket exposing (State, init)

import Ports
import WebSocketClient exposing (Config, makeConfig, makeState)


type alias State msg =
    WebSocketClient.State msg


config : Config msg
config =
    makeConfig Ports.webSocketClientCmd


init : State msg
init =
    makeState config
