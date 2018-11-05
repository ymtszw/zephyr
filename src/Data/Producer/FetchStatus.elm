module Data.Producer.FetchStatus exposing
    ( FetchStatus(..), Backoff(..), RecentError(..)
    , encode, decoder, compare, lessThan, subscribed
    )

{-| Status of Periodic fetch.

@docs FetchStatus, Backoff, RecentError
@docs encode, decoder, compare, lessThan, subscribed
@docs Msg, update

-}

import Extra exposing (ite)
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)
import TimeExtra exposing (ms, posix)


type FetchStatus
    = Subscribed
    | NextFetchAt Posix Backoff
    | Fetching Posix Backoff
    | InitialFetching -- If unexpectedly failed, should go Unavailable, allow retry
    | Available -- Initial state; can transit to Subscribed => InitialFetching => NextFetchAt/Unavailble
    | Unavailable RecentError -- May retry, but should be listed at hard-to-reach position


type Backoff
    = BO2
    | BO5
    | BO10
    | BO30
    | BO60
    | BO120


type RecentError
    = Unexpected -- Transient ones
    | Forbidden


encode : FetchStatus -> E.Value
encode fetchStatus =
    case fetchStatus of
        Subscribed ->
            E.tag "Subscribed"

        NextFetchAt posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        Fetching posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        InitialFetching ->
            E.tag "Subscribed"

        Available ->
            E.tag "Available"

        Unavailable Unexpected ->
            E.tagged "Unavailable" (E.tag "Unexpected")

        Unavailable Forbidden ->
            E.tagged "Unavailable" (E.tag "Forbidden")


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
        [ D.tag "Subscribed" Subscribed
        , D.tagged2 "NextFetchAt" NextFetchAt (D.map posix D.int) backoffDecoder
        , D.tag "Available" Available
        , D.tagged "Unavailable" Unavailable <|
            D.oneOf [ D.tag "Unexpected" Unexpected, D.tag "Forbidden" Forbidden ]

        -- Old format
        , D.tag "NeverFetched" Available
        , D.tag "Waiting" Subscribed
        , D.tag "Forbidden" (Unavailable Forbidden)
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
            ( Subscribed, _ ) ->
                LT

            ( NextFetchAt _ _, Subscribed ) ->
                GT

            ( NextFetchAt p1 _, NextFetchAt p2 _ ) ->
                -- For ease of impl., not comparing current backoff
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( NextFetchAt _ _, _ ) ->
                LT

            ( Fetching _ _, Subscribed ) ->
                GT

            ( Fetching _ _, NextFetchAt _ _ ) ->
                GT

            ( Fetching p1 _, Fetching p2 _ ) ->
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( Fetching _ _, _ ) ->
                LT

            ( InitialFetching, Available ) ->
                LT

            ( InitialFetching, Unavailable _ ) ->
                LT

            ( InitialFetching, _ ) ->
                GT

            ( Available, Unavailable _ ) ->
                LT

            ( Available, _ ) ->
                GT

            ( Unavailable Unexpected, Unavailable Forbidden ) ->
                LT

            ( Unavailable _, _ ) ->
                GT


{-| True if the second one is less than the first one (pipeline-friendly order).
-}
lessThan : FetchStatus -> FetchStatus -> Bool
lessThan a b =
    compare b a == LT


subscribed : FetchStatus -> Bool
subscribed fetchStatus =
    case fetchStatus of
        Subscribed ->
            True

        NextFetchAt _ _ ->
            True

        Fetching _ _ ->
            True

        InitialFetching ->
            False

        Available ->
            False

        Unavailable _ ->
            False


type Msg
    = Sub
    | Unsub
    | Start
    | Hit Posix
    | Miss Posix
    | Fail RecentError


update : Msg -> FetchStatus -> FetchStatus
update msg fs =
    case msg of
        Sub ->
            ite (subscribed fs) fs Subscribed

        Unsub ->
            ite (subscribed fs) Available fs

        Start ->
            case fs of
                Subscribed ->
                    InitialFetching

                NextFetchAt posix bo ->
                    Fetching posix bo

                _ ->
                    fs

        Hit posix ->
            case fs of
                Fetching _ _ ->
                    NextFetchAt (TimeExtra.add 2000 posix) BO2

                InitialFetching ->
                    NextFetchAt (TimeExtra.add 2000 posix) BO2

                _ ->
                    fs

        Miss posix ->
            case fs of
                Fetching _ bo ->
                    backoff bo posix

                InitialFetching ->
                    backoff BO2 posix

                _ ->
                    fs

        Fail recentError ->
            Unavailable recentError


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
