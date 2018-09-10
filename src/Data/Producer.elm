module Data.Producer exposing (Input(..), Producer, Storage, installAll)

import Data.Item exposing (Item)
import Data.UniqueId as UniqueId
import Dict exposing (Dict)
import Websocket exposing (Key(..))


type Producer
    = Producer { name : String, input : Input }


type Input
    = Realtime Websocket.Endpoint (Websocket.EventHandler Storage Item)
    | Polling


type alias Storage =
    Dict String String


type alias InstallAction msg =
    { idGen : UniqueId.Generator
    , wsState : Websocket.State msg Storage Item
    , cmd : Cmd msg
    }


installAll : UniqueId.Generator -> Websocket.State msg Storage Item -> List Producer -> InstallAction msg
installAll idGen wsState producers =
    List.foldl install (InstallAction idGen wsState Cmd.none) producers


install : Producer -> InstallAction msg -> InstallAction msg
install (Producer { name, input }) acc =
    case input of
        Realtime endpoint handler ->
            let
                ( keyStr, newIdGen ) =
                    UniqueId.gen name acc.idGen

                ( newWsState, cmd ) =
                    Websocket.engage acc.wsState (Key keyStr) endpoint handler
            in
            { acc | idGen = newIdGen, wsState = newWsState, cmd = Cmd.batch [ cmd, acc.cmd ] }

        Polling ->
            -- NYI
            acc
