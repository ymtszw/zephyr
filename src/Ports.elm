port module Ports exposing (indexedDBError, loadFromJs, sendToJs)

import Json.Encode exposing (Value)


port loadFromJs : (Value -> msg) -> Sub msg


port indexedDBError : (Value -> msg) -> Sub msg


port sendToJs : Value -> Cmd msg
