port module Ports exposing (loadFromJs, sendToJs)

import Json.Encode as E


port loadFromJs : (E.Value -> msg) -> Sub msg


port sendToJs : E.Value -> Cmd msg
