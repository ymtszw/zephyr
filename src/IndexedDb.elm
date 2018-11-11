port module IndexedDb exposing
    ( ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry
    , load, postUpdate, noPersist
    )

{-| Handles persistence of application state to IndexedDB.

Follow the best practices!!
<https://developers.google.com/web/fundamentals/instant-and-offline/web-storage/indexeddb-best-practices>

@docs ChangeSet, changeSet, saveColumnStore, saveItemBroker, saveProducerRegistry
@docs load, postUpdate, noPersist

-}

import Data.ColumnStore
import Data.ItemBroker
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer
import Data.SavedState as SavedState
import Data.Storable exposing (Storable)
import Data.UniqueIdGen as UniqueIdGen exposing (UniqueIdGen)
import Json.Decode as D
import Json.Encode as E


load : Int -> UniqueIdGen -> Sub Msg
load clientHeight idGen =
    loadFromJs (loadMsg clientHeight idGen)


loadMsg : Int -> UniqueIdGen -> E.Value -> Msg
loadMsg clientHeight idGen value =
    case D.decodeValue (SavedState.decoder clientHeight idGen) value of
        Ok savedState ->
            LoadOk savedState

        Err e ->
            LoadErr e


port loadFromJs : (E.Value -> msg) -> Sub msg


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
                Data.Storable.append [ ( "idGen", UniqueIdGen.encode m.idGen ) ] <|
                    Data.ColumnStore.encode m.columnStore
    , toCmd cs.itemBroker <|
        \_ ->
            doPersist (Data.ItemBroker.encode m.itemBroker)
    , toCmd cs.producerRegistry <|
        \_ ->
            doPersist (Data.Producer.encodeRegistry m.producerRegistry)
    ]
        |> List.filterMap identity


toCmd : Bool -> (() -> Cmd Msg) -> Maybe (Cmd Msg)
toCmd switch genCmd =
    if switch then
        Just (genCmd ())

    else
        Nothing


doPersist : Storable -> Cmd msg
doPersist storable =
    sendToJs (Data.Storable.finalize storable)


port sendToJs : E.Value -> Cmd msg


{-| A Glue function that connects ordinary component update output into postUpdate.
-}
noPersist : ( model, Cmd msg ) -> ( model, Cmd msg, ChangeSet )
noPersist ( m, cmd ) =
    ( m, cmd, changeSet )
