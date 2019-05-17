module Data.Producer.FetchStatus exposing
    ( FetchStatus(..), Backoff(..)
    , encode, decoder, compare, lessThan, fetching, subscribed, dormant
    , Msg(..), update
    )

{-| Status of Periodic fetch.

On non-transient error, the fetch subject itself should be removed.
It may be re-introduced/retried by manual actions (such as Discord's rehydration.)

@docs FetchStatus, Backoff, RecentError
@docs encode, decoder, compare, lessThan, fetching, subscribed, dormant
@docs Msg, update

-}

import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)
import TimeExtra exposing (ms)


type FetchStatus
    = Waiting
    | NextFetchAt Posix Backoff
    | Fetching Posix Backoff
    | InitialFetching Posix -- If failed in unrecoverable way (Forbidden/NotFound), host object itself should be removed/disabled
    | Available -- Initial state; can transit to Waiting => InitialFetching => NextFetchAt (or dropped)


type
    Backoff
    -- Values of backoff must be carefully studied so that it won't cause Too Many Requests AND is responsive enough
    = BO10
    | BO20
    | BO40


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
        BO10 ->
            E.tag "BO10"

        BO20 ->
            E.tag "BO20"

        BO40 ->
            E.tag "BO40"


decoder : Decoder FetchStatus
decoder =
    D.oneOf
        [ D.tag "Waiting" Waiting
        , D.tagged2 "NextFetchAt" NextFetchAt (D.map Time.millisToPosix D.int) backoffDecoder
        , D.tag "Available" Available

        -- Old format
        , D.tag "NeverFetched" Available
        , D.tag "Forbidden" Available -- Purged when tried
        ]


backoffDecoder : Decoder Backoff
backoffDecoder =
    D.oneOf
        [ D.tag "BO10" BO10
        , D.tag "BO20" BO20
        , D.tag "BO40" BO40

        -- For deprecated patterns
        , D.when (D.field "tag" D.string) (String.startsWith "BO") (D.succeed BO10)
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


fetching : FetchStatus -> Bool
fetching fetchStatus =
    case fetchStatus of
        Fetching _ _ ->
            True

        InitialFetching _ ->
            True

        _ ->
            False


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
    | Spur Posix


update : Msg -> FetchStatus -> { fs : FetchStatus, persist : Bool, triggerRefresh : Bool }
update msg fs =
    let
        pure fs_ =
            { fs = fs_, persist = False, triggerRefresh = False }
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
                    { fs = Available, persist = True, triggerRefresh = True }

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
                    { fs = NextFetchAt (TimeExtra.add 10000 posix) BO10, persist = True, triggerRefresh = False }

                InitialFetching _ ->
                    { fs = NextFetchAt (TimeExtra.add 10000 posix) BO10, persist = True, triggerRefresh = True }

                _ ->
                    pure fs

        Miss posix ->
            case fs of
                Fetching _ bo ->
                    { fs = backoff bo posix, persist = True, triggerRefresh = False }

                InitialFetching _ ->
                    { fs = backoff BO10 posix, persist = True, triggerRefresh = True }

                _ ->
                    pure fs

        Fail ->
            case fs of
                Fetching posix bo ->
                    { fs = backoff bo posix, persist = True, triggerRefresh = False }

                InitialFetching _ ->
                    -- This is very rare, but can happen.
                    -- If it stacks at InitialFetching <-> Waiting, should be cancelled by Unsub
                    pure Waiting

                _ ->
                    -- This includes other ChannelAPIError such as BadRequest on Create Message
                    pure fs

        Spur posix ->
            case fs of
                Waiting ->
                    -- Already at front of the queue
                    pure fs

                NextFetchAt _ _ ->
                    { fs = NextFetchAt posix BO10, persist = True, triggerRefresh = False }

                _ ->
                    pure fs


backoff : Backoff -> Posix -> FetchStatus
backoff bo posix =
    case bo of
        BO10 ->
            NextFetchAt (TimeExtra.add 10000 posix) BO20

        BO20 ->
            NextFetchAt (TimeExtra.add 20000 posix) BO40

        BO40 ->
            NextFetchAt (TimeExtra.add 40000 posix) BO40
