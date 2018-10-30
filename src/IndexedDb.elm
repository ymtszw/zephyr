port module IndexedDb exposing (load, noPersist, postUpdate)

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


{-| A hook to persist Elm application state to IndexedDB via port.

It is intended to be called with specialized update output,
which has third tuple element instructing this function
whether the model should be persisted or not.

TODO change shouldPersist Bool into PersistenceInstruction record,
telling which portion of the Model should be persisted,
with breaking SavedState into separate IndexedDB objects.
So that parts of the Model can be efficiently saved/loaded.

-}
postUpdate : ( Model, Cmd Msg, Bool ) -> ( Model, Cmd Msg )
postUpdate ( model, cmd, shouldPersist ) =
    ( model
    , if model.env.indexedDBAvailable && shouldPersist then
        Cmd.batch [ cmd, sendToJs (Model.encodeForPersistence model) ]

      else
        cmd
    )


port sendToJs : E.Value -> Cmd msg


{-| A Glue function that connects ordinary component update output into postUpdate.
-}
noPersist : ( model, Cmd msg ) -> ( model, Cmd msg, Bool )
noPersist ( m, cmd ) =
    ( m, cmd, False )
