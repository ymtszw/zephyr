module Data.Producer.Polling exposing (Update, Yield, save)

{-| Defines types used by Producers that periodically
fetch contents from remote APIs.
-}

import Data.Item exposing (Item)


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
