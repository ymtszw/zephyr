port module Ports exposing (loadFromJs, sendToJs)

import Json.Encode exposing (Value)


port loadFromJs : (Value -> msg) -> Sub msg


port sendToJs : Value -> Cmd msg
