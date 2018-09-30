module Data.Producer.Base exposing (Update, Yield, save, setTimeout)

{-| Defines types used by Producers.
-}

import Data.Item exposing (Item)
import Process
import Task


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


setTimeout : msg -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep timeout |> Task.perform (\_ -> timeoutMsg)
