module Data.Producer.Base exposing
    ( Yield, Reload, PostProcess, UpdateFAM(..)
    , enter, enterAndFire, yield, yieldAndFire, destroy
    )

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.


## Types

@docs Yield, Reload, PostProcess, UpdateFAM


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
type alias Yield item mat state msg =
    { items : List item
    , postProcess : PostProcess mat
    , newState : Maybe state
    , cmd : Cmd msg
    }


type alias PostProcess mat =
    { persist : Bool
    , updateFAM : UpdateFAM mat
    }


type UpdateFAM mat
    = SetFAM mat
    | KeepFAM
    | DestroyFAM


{-| Return type of Producer's reload function, called on application startup.

It is not allowed to destroy Producer state on reload.
And always new state is persisted, in order to apply new encoding format, if any.

-}
type alias Reload mat state msg =
    ( state, Cmd msg, UpdateFAM mat )


{-| Just entering a specific state of a Producer.
-}
enter : PostProcess mat -> state -> Yield item mat state msg
enter pp state =
    Yield [] pp (Just state) Cmd.none


{-| Enters a specific state, and fire an event (Cmd msg).
-}
enterAndFire : PostProcess mat -> state -> Cmd msg -> Yield item mat state msg
enterAndFire pp state cmd =
    Yield [] pp (Just state) cmd


{-| Yield items and enter a state. Always persist.
-}
yield : List item -> UpdateFAM mat -> state -> Yield item mat state msg
yield items updateFAM state =
    Yield items (PostProcess True updateFAM) (Just state) Cmd.none


{-| Yield items, enter a state, and fire an event. Always persist.
-}
yieldAndFire : List item -> UpdateFAM mat -> state -> Cmd msg -> Yield item mat state msg
yieldAndFire items updateFAM state cmd =
    Yield items (PostProcess True updateFAM) (Just state) cmd


{-| Destroys the whole state machine. Discarding/deregistering a Producer.

Always persist, and destroy FilterAtomMaterial.

-}
destroy : Yield item mat state msg
destroy =
    Yield [] (PostProcess True DestroyFAM) Nothing Cmd.none
