module Data.Producer.Base exposing (Yield, UpdateFAM(..), yield, pure)

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.

@docs Yield, UpdateFAM, yield, pure

-}

import Worque exposing (Work)


{-| Return type of side-effects from Producer's update/reload function.

Returned in pair with new Producer states.

In state machine terminology, `cmd` corresponds to "events".

Generated `items` are considered side-effect of state machine transitions.
Items must be ordered **from oldest to latest**.

On reload, items are most likely empty.
Also, regardless of persist instruction, the newState will be persisted,
in order to apply new encoding format (if any).

-}
type alias Yield item mat msg =
    { cmd : Cmd msg
    , persist : Bool
    , items : List item
    , updateFAM : UpdateFAM mat
    , work : Maybe Work
    }


type UpdateFAM mat
    = SetFAM mat
    | KeepFAM
    | DestroyFAM


{-| Default Yield. No side-effect at all.
-}
yield : Yield item mat msg
yield =
    { cmd = Cmd.none, persist = False, items = [], updateFAM = KeepFAM, work = Nothing }


{-| Just entering a specific state of a Producer, without any sort of side-effect.
-}
pure : state -> ( state, Yield item mat msg )
pure state =
    ( state, yield )
