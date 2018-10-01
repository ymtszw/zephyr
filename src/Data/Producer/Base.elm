module Data.Producer.Base exposing (Reload, Update, Yield, save, setTimeout)

{-| Defines types used by Producers.

Producers can be of two types: Polling and Realtime.
Realtime Producer is based on Websocket event handling (to be refactored).
There could be Hybrid Producer introduced later.

-}

import Data.Item exposing (Item)
import Process
import Task


{-| Return type of Producer's update function.

Returning `Nothing` as a new state triggers deregistering of the Producer.

-}
type alias Yield state msg =
    ( List Item, Maybe state, Cmd msg )


{-| Convenient helper to just save new Producer state,
without producing any Items or Cmds.
-}
save : Maybe state -> Yield state msg
save stateMaybe =
    ( [], stateMaybe, Cmd.none )


{-| Function to handle arrived msg for a Producer.
-}
type alias Update state msg =
    msg -> Maybe state -> Yield state msg


{-| Funciton to reload a Producer on application startup.

TODO Realtime Producer needs some love.

-}
type alias Reload state msg =
    state -> ( state, Cmd msg )


setTimeout : msg -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep timeout |> Task.perform (\_ -> timeoutMsg)
