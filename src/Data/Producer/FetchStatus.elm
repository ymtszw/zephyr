module Data.Producer.FetchStatus exposing (Backoff(..), FetchStatus(..), compare, decoder, encode, lessThan, toString)

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)


type FetchStatus
    = NeverFetched
    | InitialFetching
    | NextFetchAt Posix Backoff
    | Fetching Posix Backoff
    | Forbidden


type Backoff
    = BO5
    | BO10
    | BO30
    | BO60
    | BO120


toString : FetchStatus -> String
toString fetchStatus =
    case fetchStatus of
        NeverFetched ->
            "Never fetched"

        InitialFetching ->
            "Fetching for the first time"

        NextFetchAt posix _ ->
            "Next fetch at: " ++ Iso8601.fromTime posix

        Fetching posix _ ->
            "Fetching: " ++ Iso8601.fromTime posix

        Forbidden ->
            "Forbidden"


encode : FetchStatus -> E.Value
encode fetchStatus =
    case fetchStatus of
        NeverFetched ->
            E.tag "NeverFetched"

        InitialFetching ->
            E.tag "NeverFetched"

        NextFetchAt posix bf ->
            E.tagged2 "NextFetchAt" (E.int (Time.posixToMillis posix)) (encodeBackoffFactor bf)

        Fetching posix bf ->
            E.tagged2 "NextFetchAt" (E.int (Time.posixToMillis posix)) (encodeBackoffFactor bf)

        Forbidden ->
            E.tag "Forbidden"


encodeBackoffFactor : Backoff -> E.Value
encodeBackoffFactor bf =
    case bf of
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
        [ D.tag "NeverFetched" NeverFetched
        , D.tagged2 "NextFetchAt" NextFetchAt (D.map Time.millisToPosix D.int) backoffFactorDecoder
        , D.tag "Forbidden" Forbidden
        ]


backoffFactorDecoder : Decoder Backoff
backoffFactorDecoder =
    D.oneOf [ D.tag "BO5" BO5, D.tag "BO10" BO10, D.tag "BO30" BO30, D.tag "BO60" BO60, D.tag "BO120" BO120 ]


compare : FetchStatus -> FetchStatus -> Order
compare a b =
    -- XXX demands tests!
    -- Note: List.sortWith sorts items from lowest to hightest
    if a == b then
        EQ

    else
        case ( a, b ) of
            ( NeverFetched, _ ) ->
                LT

            ( NextFetchAt _ _, NeverFetched ) ->
                GT

            ( NextFetchAt p1 _, NextFetchAt p2 _ ) ->
                -- For ease of impl., not comparing current backoff
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( NextFetchAt _ _, _ ) ->
                LT

            ( InitialFetching, Fetching _ _ ) ->
                LT

            ( InitialFetching, Forbidden ) ->
                LT

            ( InitialFetching, _ ) ->
                GT

            ( Fetching p1 _, Fetching p2 _ ) ->
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( Fetching _ _, Forbidden ) ->
                LT

            ( Fetching _ _, _ ) ->
                GT

            ( Forbidden, _ ) ->
                GT


{-| True if the second one is less than the first one (pipeline-friendly order).
-}
lessThan : FetchStatus -> FetchStatus -> Bool
lessThan a b =
    compare b a == LT