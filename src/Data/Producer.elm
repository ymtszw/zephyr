module Data.Producer exposing (Yield, yield, pure)

{-| Defines types and helpers used by Producers.

Currently Producers work by polling their API endpoints.

Realtime Producers could be implemented based on Websocket event handling.
Also there could be Hybrid of the two, utilizing both downstream event handling
AND stateless API requests.

@docs Yield, yield, pure

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
type alias Yield item s msg =
    { cmd : Cmd msg
    , persist : Bool
    , items : List item
    , availableSources : Maybe (List s)
    , work : Maybe Work
    }


{-| Default Yield. No side-effect at all.
-}
yield : Yield item s msg
yield =
    { cmd = Cmd.none, persist = False, items = [], availableSources = Nothing, work = Nothing }


{-| Just entering a specific state of a Producer, without any sort of side-effect.
-}
pure : state -> ( state, Yield item s msg )
pure state =
    ( state, yield )
