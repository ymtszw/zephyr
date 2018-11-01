module Data.Producer.Base exposing
    ( YieldBase, PostProcessBase, UpdateFAM(..)
    , pure, enter, enterAndFire, yield, yieldAndFire, destroy
    , noop
    )

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.


## Types

@docs YieldBase, Reload, PostProcessBase, UpdateFAM


## State Machine APIs

@docs pure, enter, enterAndFire, yield, yieldAndFire, destroy


## PostProcessBase APIs

@docs noop

-}

import Worque exposing (Work)


{-| Return type of Producer's update/reload function.

Returning `Nothing` as a new state triggers deregistering/discarding of the Producer.

Each `state` can be arbitrary data structure. Though usually they take forms of state machines.

In state machine terminology, `state` is literally state machine "states",
and `cmd` corresponds to "events".

Generated `items` are considered side-effect of state machine transitions.
Items must be ordered **from oldest to latest**.

On reload, items are most likely empty, and newState should not be Nothing (destroy).
Also, regardless of persist instruction, the newState will be persisted,
in order to apply new encoding format (if any).

-}
type alias YieldBase item mat state msg =
    { items : List item
    , postProcess : PostProcessBase mat
    , newState : Maybe state
    , cmd : Cmd msg
    }


type alias PostProcessBase mat =
    { persist : Bool
    , updateFAM : UpdateFAM mat
    , work : Maybe Work
    }


type UpdateFAM mat
    = SetFAM mat
    | KeepFAM
    | DestroyFAM


{-| No PostProcessBase. Meaning, no persist, KeepFAM, no Work.
-}
noop : PostProcessBase mat
noop =
    PostProcessBase False KeepFAM Nothing


{-| Just entering a specific state of a Producer, without any sort of side-effect.
Meaning, no persist, KeepFAM, no Work.
-}
pure : state -> YieldBase item mat state msg
pure state =
    YieldBase [] noop (Just state) Cmd.none


{-| Just entering a specific state of a Producer, with PostProcessBase.
-}
enter : PostProcessBase mat -> state -> YieldBase item mat state msg
enter pp state =
    YieldBase [] pp (Just state) Cmd.none


{-| Enters a specific state, and fire an event (Cmd msg).
-}
enterAndFire : PostProcessBase mat -> state -> Cmd msg -> YieldBase item mat state msg
enterAndFire pp state cmd =
    YieldBase [] pp (Just state) cmd


{-| YieldBase items and enter a state. Always persist.
-}
yield : List item -> UpdateFAM mat -> Maybe Work -> state -> YieldBase item mat state msg
yield items updateFAM work state =
    YieldBase items (PostProcessBase True updateFAM work) (Just state) Cmd.none


{-| YieldBase items, enter a state, and fire an event. Always persist.
-}
yieldAndFire : List item -> UpdateFAM mat -> Maybe Work -> state -> Cmd msg -> YieldBase item mat state msg
yieldAndFire items updateFAM work state cmd =
    YieldBase items (PostProcessBase True updateFAM work) (Just state) cmd


{-| Destroys the whole state machine. Discarding/deregistering a Producer.

Always persist, and destroy FilterAtomMaterial.

-}
destroy : YieldBase item mat state msg
destroy =
    YieldBase [] (PostProcessBase True DestroyFAM Nothing) Nothing Cmd.none
