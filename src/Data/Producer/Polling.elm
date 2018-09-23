module Data.Producer.Polling exposing (Instruction)

{-| Defines types used by Producers that periodically
fetch contents from remote APIs.
-}


{-| Instruction data that schedules next action.

Action can be specified by opaque message defined in each Producer module.

-}
type Instruction msg
    = Timeout msg Float
    | NoOp


{-| Function to handle arrived msg for a Producer.
-}
type alias Handler msg =
    state -> msg -> ( List Item, state, Instruction msg )
