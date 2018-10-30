port module IndexedDb exposing (load, save)

{-| Handles persistence of application state to IndexedDB.

Follow the best practices!!
<https://developers.google.com/web/fundamentals/instant-and-offline/web-storage/indexeddb-best-practices>

-}

import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.SavedState as SavedState
import Data.UniqueId as UniqueId
import Json.Decode as D
import Json.Encode as E


load : UniqueId.Generator -> Sub Msg
load idGen =
    loadFromJs (loadMsg idGen)


loadMsg : UniqueId.Generator -> E.Value -> Msg
loadMsg idGen value =
    case D.decodeValue (SavedState.decoder idGen) value of
        Ok savedState ->
            LoadOk savedState

        Err e ->
            LoadErr e


port loadFromJs : (E.Value -> msg) -> Sub msg


save : Model -> Cmd msg
save =
    Model.encodeForPersistence >> sendToJs


port sendToJs : E.Value -> Cmd msg
