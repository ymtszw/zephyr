module Worque exposing (Work(..), Worque, init, pop, push, pushAll)

{-| Serializer of Works (chunk of things to do that may require some time).

It is just a (Double-ended) FIFO Queue.
Make sure latest one is always saved in Model after pushed/popped.

-}

import Deque exposing (Deque)
import Logger.Entry exposing (Entry)


type Worque
    = Worque (Deque Work)


{-| Literally, a token that indicates "there is a work".
-}
type Work
    = BrokerScan Int
    | DiscordFetch
    | DropOldState
    | BrokerCatchUp String


init : Worque
init =
    Worque Deque.empty


push : Work -> Worque -> Worque
push w (Worque d) =
    Worque (Deque.pushFront w d)


pushAll : List Work -> Worque -> Worque
pushAll works worque =
    case works of
        [] ->
            worque

        w :: ws ->
            pushAll ws (push w worque)


pop : Worque -> ( Maybe Work, Worque )
pop (Worque d) =
    Deque.popBack d |> Tuple.mapSecond Worque
