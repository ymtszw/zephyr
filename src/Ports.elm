port module Ports exposing (sendToJs)

import Json.Encode exposing (Value)


port sendToJs : Value -> Cmd msg
