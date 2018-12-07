module Data.Producer.Base exposing
    ( YieldBase, PostProcessBase, UpdateFAM(..)
    , pure, enter, enterAndFire, yield, yieldAndFire
    , ppBase
    )

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.

@docs YieldBase, Reload, PostProcessBase, UpdateFAM
@docs pure, enter, enterAndFire, yield, yieldAndFire
@docs ppBase

-}

import Worque exposing (Work)


{-| Return type of Producer's update/reload function.

Returning `Nothing` as a new state triggers deregistering/discarding of the Producer.

Each `state` can be arbitrary data structure. Though usually they take forms of state machines.

In state machine terminology, `state` is literally state machine "states",
and `cmd` corresponds to "events".

Generated `items` are considered side-effect of state machine transitions.
Items must be ordered **from oldest to latest**.

On reload, items are most likely empty.
Also, regardless of persist instruction, the newState will be persisted,
in order to apply new encoding format (if any).

-}
type alias YieldBase item mat state msg =
    { items : List item
    , postProcess : PostProcessBase mat
    , newState : state
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


{-| Default PostProcessBase. Meaning, no persist, KeepFAM, no Work.
-}
ppBase : PostProcessBase mat
ppBase =
    PostProcessBase False KeepFAM Nothing


{-| Just entering a specific state of a Producer, without any sort of side-effect.
Meaning, no persist, KeepFAM, no Work.
-}
pure : state -> YieldBase item mat state msg
pure state =
    YieldBase [] ppBase state Cmd.none


{-| Just entering a specific state of a Producer, with PostProcessBase.
-}
enter : PostProcessBase mat -> state -> YieldBase item mat state msg
enter pp state =
    YieldBase [] pp state Cmd.none


{-| Enters a specific state, and fire an event (Cmd msg).
-}
enterAndFire : PostProcessBase mat -> state -> Cmd msg -> YieldBase item mat state msg
enterAndFire pp state cmd =
    YieldBase [] pp state cmd


{-| YieldBase items and enter a state. Always persist.
-}
yield : List item -> UpdateFAM mat -> Maybe Work -> state -> YieldBase item mat state msg
yield items updateFAM work state =
    YieldBase items (PostProcessBase True updateFAM work) state Cmd.none


{-| YieldBase items, enter a state, and fire an event. Always persist.
-}
yieldAndFire : List item -> UpdateFAM mat -> Maybe Work -> state -> Cmd msg -> YieldBase item mat state msg
yieldAndFire items updateFAM work state cmd =
    YieldBase items (PostProcessBase True updateFAM work) state cmd
