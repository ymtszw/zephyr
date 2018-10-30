module Data.Producer.Base exposing
    ( Yield, Update, Reload
    , enter, enterAndFire, yield, yieldAndFire, destroy
    )

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.


## Types

@docs Yield, Update, Reload


## State Machine APIs

@docs enter, enterAndFire, yield, yieldAndFire, destroy

-}


{-| Return type of Producer's update function.

Returning `Nothing` as a new state triggers deregistering/discarding of the Producer.

Each `state` can be arbitrary data structure. Though usually they take forms of state machines.

In state machine terminology, `state` is literally state machine "states",
and `cmd` corresponds to "events".

Generated `items` are considered side-effect of state machine transitions.
Items must be ordered **from oldest to latest**.

-}
type alias Yield item state msg =
    { items : List item
    , shouldPersist : Bool
    , newState : Maybe state
    , cmd : Cmd msg
    }


{-| Just entering a specific state of a Producer.
-}
enter : Bool -> state -> Yield item state msg
enter shouldPersist state =
    Yield [] shouldPersist (Just state) Cmd.none


{-| Enters a specific state, and fire an event (Cmd msg).
-}
enterAndFire : Bool -> state -> Cmd msg -> Yield item state msg
enterAndFire shouldPersist state cmd =
    Yield [] shouldPersist (Just state) cmd


{-| Yield items and enter a state. Always persist.
-}
yield : List item -> state -> Yield item state msg
yield items state =
    Yield items True (Just state) Cmd.none


{-| Yield items, enter a state, and fire an event. Always persist.
-}
yieldAndFire : List item -> state -> Cmd msg -> Yield item state msg
yieldAndFire items state cmd =
    Yield items True (Just state) cmd


{-| Destroys the whole state machine. Discarding/deregistering a Producer. Always persist.
-}
destroy : Yield item state msg
destroy =
    Yield [] True Nothing Cmd.none


{-| Function to handle arrived msg for a Producer.
-}
type alias Update item state msg =
    msg -> Maybe state -> Yield item state msg


{-| Funciton to reload a Producer on application startup.
-}
type alias Reload state msg =
    state -> ( state, Cmd msg )
