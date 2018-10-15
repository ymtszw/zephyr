module Data.Producer.Base exposing (Reload, Update, Yield, save, setTimeout)

{-| Defines types and helpers used by Producers.

Producers can be of two types: Polling and Realtime.
Realtime Producer is based on Websocket event handling (TODO when elm/websocket is ready).
There could be Hybrid Producer introduced later.

-}

import Process
import Task


{-| Return type of Producer's update function.

Returning `Nothing` as a new state triggers deregistering of the Producer.

-}
type alias Yield item state msg =
    { items : List item
    , newState : Maybe state
    , cmd : Cmd msg
    }


{-| Convenient helper to just save new Producer state,
without producing any Items or Cmds.
-}
save : Maybe state -> Yield item state msg
save stateMaybe =
    Yield [] stateMaybe Cmd.none


{-| Function to handle arrived msg for a Producer.
-}
type alias Update item state msg =
    msg -> Maybe state -> Yield item state msg


{-| Funciton to reload a Producer on application startup.
-}
type alias Reload state msg =
    state -> ( state, Cmd msg )


setTimeout : msg -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep timeout |> Task.perform (\_ -> timeoutMsg)
