module Data.Producer.Base exposing
    ( Yield, Update, Reload
    , enter, enterAndFire, yield, yieldAndFire, destroy
    , setTimeout
    )

{-| Defines types and helpers used by Producers.

Producers can be of two types: Polling and Realtime.
Realtime Producer is based on Websocket event handling (TODO when elm/websocket is ready).
There could be Hybrid Producer introduced later.


## Types

@docs Yield, Update, Reload


## State Machine APIs

@docs enter, enterAndFire, yield, yieldAndFire, destroy


## Event helper

@docs setTimeout

-}

import Process
import Task
import Time exposing (Posix)


{-| Return type of Producer's update function.

Returning `Nothing` as a new state triggers deregistering/discarding of the Producer.

Each `state` can be arbitrary data structure. Though usually they take forms of state machines.

In state machine terminology, `state` is literally state machine "states",
and `cmd` corresponds to "events".
Generated `items` are considered side-effect of state machine transitions.

-}
type alias Yield item state msg =
    { items : List item
    , newState : Maybe state
    , cmd : Cmd msg
    }


{-| Just entering a specific state of a Producer.
-}
enter : state -> Yield item state msg
enter state =
    Yield [] (Just state) Cmd.none


{-| Enters a specific state, and fire an event (Cmd msg).
-}
enterAndFire : state -> Cmd msg -> Yield item state msg
enterAndFire state cmd =
    Yield [] (Just state) cmd


{-| Yield items and enter a state.
-}
yield : List item -> state -> Yield item state msg
yield items state =
    Yield items (Just state) Cmd.none


{-| Yield items, enter a state, and fire an event. Do all!
-}
yieldAndFire : List item -> state -> Cmd msg -> Yield item state msg
yieldAndFire items state cmd =
    Yield items (Just state) cmd


{-| Destroys the whole state machine. Discarding/deregistering a Producer.
-}
destroy : Yield item state msg
destroy =
    Yield [] Nothing Cmd.none


{-| Function to handle arrived msg for a Producer.
-}
type alias Update item state msg =
    msg -> Maybe state -> Yield item state msg


{-| Funciton to reload a Producer on application startup.
-}
type alias Reload state msg =
    state -> ( state, Cmd msg )


{-| Convenient timer.

Sleep for a set amount of time, then fire event with current posix time.

-}
setTimeout : (Posix -> msg) -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep 5000
        |> Task.andThen (\() -> Time.now)
        |> Task.perform timeoutMsg
