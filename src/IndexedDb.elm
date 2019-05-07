module IndexedDb exposing
    ( load, requestItemBroker, requestProducerRegistry, requestPref, dropOldState
    , ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry, savePref
    , postUpdate, noPersist
    )

{-| Handles persistence of application state to IndexedDB.

Follow the best practices!!
<https://developers.google.com/web/fundamentals/instant-and-offline/web-storage/indexeddb-best-practices>

@docs load, requestItemBroker, requestProducerRegistry, requestPref, dropOldState
@docs ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry, savePref
@docs postUpdate, noPersist

-}

import Data.ColumnStore as ColumnStore
import Data.ItemBroker as ItemBroker
import Data.Model exposing (Env, Model)
import Data.Msg exposing (Msg(..))
import Data.Pref as Pref
import Data.ProducerRegistry as ProducerRegistry
import Data.Storable as Storable exposing (Storable)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Ports exposing (loadFromJs, sendToJs)



-- Load State


load : Env -> Sub Msg
load env =
    loadFromJs (loadMsg env)


loadMsg : Env -> E.Value -> Msg
loadMsg env value =
    case D.decodeValue (stateDecoder env) value of
        Ok msg ->
            msg

        Err e ->
            LoadErr e


stateDecoder : Env -> Decoder Msg
stateDecoder env =
    D.do (D.field "id" D.string) <|
        \id ->
            if id == ColumnStore.storeId then
                D.map
                    (\( cs, idAndCmds ) -> LoadColumnStore ( cs, Cmd.map ColumnCtrl idAndCmds ))
                    (ColumnStore.decoder { clientHeight = env.clientHeight, posix = env.posix })

            else if id == ItemBroker.storeId then
                D.map LoadItemBroker ItemBroker.decoder

            else if id == ProducerRegistry.storeId then
                D.map LoadProducerRegistry ProducerRegistry.decoder

            else if id == Pref.storeId then
                D.map LoadPref (Pref.decoder env.clientWidth)

            else
                D.fail ("Unknown state id: " ++ id)


requestItemBroker : Cmd msg
requestItemBroker =
    requestStored ItemBroker.storeId


requestProducerRegistry : Cmd msg
requestProducerRegistry =
    requestStored ProducerRegistry.storeId


requestPref : Cmd msg
requestPref =
    requestStored Pref.storeId


requestStored : String -> Cmd msg
requestStored id =
    sendToJs (E.object [ ( "__requestId", E.string id ) ])


dropOldState : Cmd msg
dropOldState =
    sendToJs (E.object [ ( "__dropOldState", E.bool True ) ])



-- Save State


type ChangeSet
    = ChangeSet
        { columnStore : Bool
        , itemBroker : Bool
        , producerRegistry : Bool
        , pref : Bool
        }


changeSet : ChangeSet
changeSet =
    ChangeSet
        { columnStore = False
        , itemBroker = False
        , producerRegistry = False
        , pref = False
        }


saveColumnStore : ChangeSet -> ChangeSet
saveColumnStore (ChangeSet cs) =
    ChangeSet { cs | columnStore = True }


saveItemBroker : ChangeSet -> ChangeSet
saveItemBroker (ChangeSet cs) =
    ChangeSet { cs | itemBroker = True }


saveProducerRegistry : ChangeSet -> ChangeSet
saveProducerRegistry (ChangeSet cs) =
    ChangeSet { cs | producerRegistry = True }


savePref : ChangeSet -> ChangeSet
savePref (ChangeSet cs) =
    ChangeSet { cs | pref = True }


{-| A hook to persist Elm application state to IndexedDB via port.

It is intended to be called with specialized update output,
which has third tuple element instructing this function
whether the model should be persisted or not.

-}
postUpdate : ( Model, Cmd Msg, ChangeSet ) -> ( Model, Cmd Msg )
postUpdate ( model, cmd, cs ) =
    ( model
    , if model.env.indexedDBAvailable then
        case changeSetToCmds model cs of
            [] ->
                cmd

            nonEmpty ->
                Cmd.batch (cmd :: nonEmpty)

      else
        cmd
    )


changeSetToCmds : Model -> ChangeSet -> List (Cmd Msg)
changeSetToCmds m (ChangeSet cs) =
    List.filterMap identity
        [ toCmd cs.columnStore <|
            \_ ->
                doPersist (ColumnStore.encode m.columnStore)
        , toCmd cs.itemBroker <|
            \_ ->
                doPersist (ItemBroker.encode m.itemBroker)
        , toCmd cs.producerRegistry <|
            \_ ->
                doPersist (ProducerRegistry.encode m.producerRegistry)
        , toCmd cs.pref <|
            \_ ->
                doPersist (Pref.encode m.pref)
        ]


toCmd : Bool -> (() -> Cmd Msg) -> Maybe (Cmd Msg)
toCmd switch genCmd =
    if switch then
        Just (genCmd ())

    else
        Nothing


doPersist : Storable -> Cmd msg
doPersist storable =
    sendToJs (Storable.finalize storable)


{-| A Glue function that connects ordinary component update output into postUpdate.
-}
noPersist : ( model, Cmd msg ) -> ( model, Cmd msg, ChangeSet )
noPersist ( m, cmd ) =
    ( m, cmd, changeSet )
