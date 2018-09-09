port module Ports exposing (loadFromJs, sendToJs, webSocketClientCmd, webSocketClientSub)

import Json.Encode exposing (Value)


port loadFromJs : (Value -> msg) -> Sub msg


port sendToJs : Value -> Cmd msg


port webSocketClientCmd : Value -> Cmd msg


port webSocketClientSub : (Value -> msg) -> Sub msg
