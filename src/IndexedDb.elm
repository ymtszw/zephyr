port module IndexedDb exposing
    ( load, requestItemBroker, requestProducerRegistry
    , ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry, postUpdate, noPersist
    )

{-| Handles persistence of application state to IndexedDB.

Follow the best practices!!
<https://developers.google.com/web/fundamentals/instant-and-offline/web-storage/indexeddb-best-practices>

@docs load, requestItemBroker, requestProducerRegistry
@docs ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry, postUpdate, noPersist

-}

import Data.ColumnStore as ColumnStore
import Data.ItemBroker as ItemBroker
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer
import Data.SavedState as SavedState
import Data.Storable as Storable exposing (Storable)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E



-- Load State


load : Int -> UniqueIdGen -> Sub Msg
load clientHeight idGen =
    loadFromJs (loadMsg clientHeight idGen)


loadMsg : Int -> UniqueIdGen -> E.Value -> Msg
loadMsg clientHeight idGen value =
    case D.decodeValue (stateDecoder clientHeight) value of
        Ok msg ->
            msg

        Err e ->
            LoadErr e


stateDecoder : Int -> Decoder Msg
stateDecoder clientHeight =
    D.oneOf
        [ D.do (D.field "id" D.string) <|
            \id ->
                if id == ColumnStore.storeId then
                    D.map2 (\cs idGen -> LoadColumnStore ( cs, idGen ))
                        (ColumnStore.decoder clientHeight)
                        (D.field idGenStoreId UniqueIdGen.decoder)

                else if id == ItemBroker.storeId then
                    D.map LoadItemBroker ItemBroker.decoder

                else if id == Producer.registryStoreId then
                    D.map LoadProducerRegistry Producer.registryDecoder

                else
                    D.fail ("Unknown state id: " ++ id)

        -- Old format
        , D.map LoadOk <| SavedState.decoder clientHeight
        ]


requestItemBroker : Cmd msg
requestItemBroker =
    requestStored ItemBroker.storeId


requestProducerRegistry : Cmd msg
requestProducerRegistry =
    requestStored Producer.registryStoreId


requestStored : String -> Cmd msg
requestStored id =
    sendToJs (E.object [ ( "__requestId", E.string id ) ])



-- Save State


type ChangeSet
    = ChangeSet
        { columnStore : Bool
        , itemBroker : Bool
        , producerRegistry : Bool
        }


changeSet : ChangeSet
changeSet =
    ChangeSet
        { columnStore = False
        , itemBroker = False
        , producerRegistry = False
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


{-| A hook to persist Elm application state to IndexedDB via port.

It is intended to be called with specialized update output,
which has third tuple element instructing this function
whether the model should be persisted or not.

TODO change shouldPersist Bool into PersistenceInstruction record,
telling which portion of the Model should be persisted,
with breaking SavedState into separate IndexedDB objects.
So that parts of the Model can be efficiently saved/loaded.

-}
postUpdate : ( Model, Cmd Msg, ChangeSet ) -> ( Model, Cmd Msg )
postUpdate ( model, cmd, cs ) =
    ( model
    , if model.env.indexedDBAvailable then
        case changeSetToCmds model cs of
            [] ->
                cmd

            nonEmpty ->
                Cmd.batch <|
                    [ cmd
                    , sendToJs (Model.encodeForPersistence model) -- Old format
                    ]
                        ++ nonEmpty

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
