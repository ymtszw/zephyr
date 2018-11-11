module Data.Producer.FetchStatus exposing
    ( FetchStatus(..), Backoff(..)
    , encode, decoder, compare, lessThan, subscribed, dormant
    , Msg(..), update
    )

{-| Status of Periodic fetch.

On non-transient error, the fetch subject itself should be removed.
It may be re-introduced/retried by manual actions (such as Discord's rehydration.)

@docs FetchStatus, Backoff, RecentError
@docs encode, decoder, compare, lessThan, subscribed, dormant
@docs Msg, update

-}

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)
import TimeExtra exposing (ms, posix)


type FetchStatus
    = Waiting
    | NextFetchAt Posix Backoff
    | Fetching Posix Backoff
    | InitialFetching Posix -- If unexpectedly failed, should go Unavailable, allow retry
    | Available -- Initial state; can transit to Waiting => InitialFetching => NextFetchAt (or dropped)


type Backoff
    = BO2
    | BO5
    | BO10
    | BO30
    | BO60
    | BO120


encode : FetchStatus -> E.Value
encode fetchStatus =
    case fetchStatus of
        Waiting ->
            E.tag "Waiting"

        NextFetchAt posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        Fetching posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        InitialFetching _ ->
            E.tag "Waiting"

        Available ->
            E.tag "Available"


encodeBackoff : Backoff -> E.Value
encodeBackoff bo =
    case bo of
        BO2 ->
            E.tag "BO2"

        BO5 ->
            E.tag "BO5"

        BO10 ->
            E.tag "BO10"

        BO30 ->
            E.tag "BO30"

        BO60 ->
            E.tag "BO60"

        BO120 ->
            E.tag "BO120"


decoder : Decoder FetchStatus
decoder =
    D.oneOf
        [ D.tag "Waiting" Waiting
        , D.tagged2 "NextFetchAt" NextFetchAt (D.map posix D.int) backoffDecoder
        , D.tag "Available" Available

        -- Old format
        , D.tag "NeverFetched" Available
        , D.tag "Forbidden" Available -- Purged when tried
        ]


backoffDecoder : Decoder Backoff
backoffDecoder =
    D.oneOf
        [ D.tag "BO2" BO2
        , D.tag "BO5" BO5
        , D.tag "BO10" BO10
        , D.tag "BO30" BO30
        , D.tag "BO60" BO60
        , D.tag "BO120" BO120
        ]


compare : FetchStatus -> FetchStatus -> Order
compare a b =
    -- Note: List.sortWith sorts items from lowest to hightest
    if a == b then
        EQ

    else
        case ( a, b ) of
            ( Waiting, _ ) ->
                LT

            ( NextFetchAt _ _, Waiting ) ->
                GT

            ( NextFetchAt p1 _, NextFetchAt p2 _ ) ->
                -- For ease of impl., not comparing current backoff
                Basics.compare (ms p1) (ms p2)

            ( NextFetchAt _ _, _ ) ->
                LT

            ( Fetching _ _, Waiting ) ->
                GT

            ( Fetching _ _, NextFetchAt _ _ ) ->
                GT

            ( Fetching p1 _, Fetching p2 _ ) ->
                Basics.compare (ms p1) (ms p2)

            ( Fetching _ _, _ ) ->
                LT

            ( InitialFetching p1, InitialFetching p2 ) ->
                Basics.compare (ms p1) (ms p2)

            ( InitialFetching _, Available ) ->
                LT

            ( InitialFetching _, _ ) ->
                GT

            ( Available, _ ) ->
                GT


{-| True if the second one is less than the first one (pipeline-friendly order).
-}
lessThan : FetchStatus -> FetchStatus -> Bool
lessThan a b =
    compare b a == LT


subscribed : FetchStatus -> Bool
subscribed fetchStatus =
    case fetchStatus of
        Waiting ->
            -- Consider as subscribed only after initial fetch succeeded
            False

        NextFetchAt _ _ ->
            True

        Fetching _ _ ->
            True

        InitialFetching _ ->
            False

        Available ->
            False


dormant : FetchStatus -> Bool
dormant fetchStatus =
    case fetchStatus of
        Available ->
            True

        _ ->
            False


type Msg
    = Sub
    | Unsub
    | Start Posix
    | Hit Posix
    | Miss Posix
    | Fail


update : Msg -> FetchStatus -> { fs : FetchStatus, persist : Bool, updateFAM : Bool }
update msg fs =
    let
        pure fs_ =
            { fs = fs_, persist = False, updateFAM = False }
    in
    case msg of
        Sub ->
            case fs of
                Available ->
                    pure Waiting

                _ ->
                    pure fs

        Unsub ->
            case fs of
                Available ->
                    pure fs

                _ ->
                    { fs = Available, persist = True, updateFAM = True }

        Start posix ->
            case fs of
                Waiting ->
                    pure (InitialFetching posix)

                NextFetchAt _ bo ->
                    pure (Fetching posix bo)

                _ ->
                    pure fs

        Hit posix ->
            case fs of
                Fetching _ _ ->
                    { fs = NextFetchAt (TimeExtra.add 2000 posix) BO2, persist = True, updateFAM = False }

                InitialFetching _ ->
                    { fs = NextFetchAt (TimeExtra.add 2000 posix) BO2, persist = True, updateFAM = True }

                _ ->
                    pure fs

        Miss posix ->
            case fs of
                Fetching _ bo ->
                    { fs = backoff bo posix, persist = True, updateFAM = False }

                InitialFetching _ ->
                    { fs = backoff BO2 posix, persist = True, updateFAM = True }

                _ ->
                    pure fs

        Fail ->
            case fs of
                Fetching posix _ ->
                    { fs = backoff BO30 posix, persist = True, updateFAM = False }

                InitialFetching _ ->
                    -- This is very rare, but can happen.
                    -- If it stacks at InitialFetching <-> Waiting, should be cancelled by Unsub
                    pure Waiting

                _ ->
                    pure fs


backoff : Backoff -> Posix -> FetchStatus
backoff bo posix =
    case bo of
        BO2 ->
            NextFetchAt (TimeExtra.add 2000 posix) BO5

        BO5 ->
            NextFetchAt (TimeExtra.add 5000 posix) BO10

        BO10 ->
            NextFetchAt (TimeExtra.add 10000 posix) BO30

        BO30 ->
            NextFetchAt (TimeExtra.add 30000 posix) BO60

        BO60 ->
            NextFetchAt (TimeExtra.add 60000 posix) BO120

        BO120 ->
            NextFetchAt (TimeExtra.add 120000 posix) BO120
