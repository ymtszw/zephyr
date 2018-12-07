port module IndexedDb exposing
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
import Data.Model as Model exposing (Env, Model)
import Data.Msg exposing (Msg(..))
import Data.Pref as Pref
import Data.Producer as Producer
import Data.SavedState as SavedState
import Data.Storable as Storable exposing (Storable)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E



-- Load State


load : Env -> UniqueIdGen -> Sub Msg
load env idGen =
    loadFromJs (loadMsg env idGen)


loadMsg : Env -> UniqueIdGen -> E.Value -> Msg
loadMsg env idGen value =
    case D.decodeValue (stateDecoder env) value of
        Ok msg ->
            msg

        Err e ->
            LoadErr e


stateDecoder : Env -> Decoder Msg
stateDecoder env =
    D.oneOf
        [ D.do (D.field "id" D.string) <|
            \id ->
                if id == ColumnStore.storeId then
                    D.map2
                        (\( cs, idAndCmds ) idGen ->
                            let
                                initCmd =
                                    Cmd.batch <| List.map (\( cId, cCmd ) -> Cmd.map (ColumnCtrl cId) cCmd) <| idAndCmds
                            in
                            LoadColumnStore ( cs, idGen, initCmd )
                        )
                        (ColumnStore.decoder env.clientHeight)
                        (D.field idGenStoreId UniqueIdGen.decoder)

                else if id == ItemBroker.storeId then
                    D.map LoadItemBroker ItemBroker.decoder

                else if id == Producer.registryStoreId then
                    D.map LoadProducerRegistry Producer.registryDecoder

                else if id == Pref.storeId then
                    D.map LoadPref (Pref.decoder env.clientWidth)

                else
                    D.fail ("Unknown state id: " ++ id)

        -- Old format; may remove after migration
        , D.map LoadOk <| SavedState.decoder env.clientHeight
        ]


requestItemBroker : Cmd msg
requestItemBroker =
    requestStored ItemBroker.storeId


requestProducerRegistry : Cmd msg
requestProducerRegistry =
    requestStored Producer.registryStoreId


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
    [ toCmd cs.columnStore <|
        \_ ->
            doPersist <|
                Storable.append [ ( idGenStoreId, UniqueIdGen.encode m.idGen ) ] <|
                    ColumnStore.encode m.columnStore
    , toCmd cs.itemBroker <|
        \_ ->
            doPersist (ItemBroker.encode m.itemBroker)
    , toCmd cs.producerRegistry <|
        \_ ->
            doPersist (Producer.encodeRegistry m.producerRegistry)
    , toCmd cs.pref <|
        \_ ->
            doPersist (Pref.encode m.pref)
    ]
        |> List.filterMap identity


idGenStoreId : String
idGenStoreId =
    "idGen"


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



-- Ports


port loadFromJs : (E.Value -> msg) -> Sub msg


port sendToJs : E.Value -> Cmd msg