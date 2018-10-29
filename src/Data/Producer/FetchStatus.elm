module Data.Producer.FetchStatus exposing (Backoff(..), FetchStatus(..), compare, decoder, encode, isActive, isAvailable, lessThan)

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Time exposing (Posix)
import TimeExtra exposing (ms, posix)


type FetchStatus
    = NeverFetched
    | Waiting
    | NextFetchAt Posix Backoff
    | InitialFetching
    | ResumeFetching
    | Fetching Posix Backoff
    | Available
    | Forbidden


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
        NeverFetched ->
            E.tag "NeverFetched"

        Waiting ->
            E.tag "Waiting"

        NextFetchAt posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        InitialFetching ->
            E.tag "NeverFetched"

        ResumeFetching ->
            E.tag "Waiting"

        Fetching posix bo ->
            E.tagged2 "NextFetchAt" (E.int (ms posix)) (encodeBackoff bo)

        Available ->
            E.tag "Available"

        Forbidden ->
            E.tag "Forbidden"


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
        [ D.tag "NeverFetched" NeverFetched
        , D.tag "Waiting" Waiting
        , D.tagged2 "NextFetchAt" NextFetchAt (D.map posix D.int) backoffDecoder
        , D.tag "Available" Available
        , D.tag "Forbidden" Forbidden
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
            ( NeverFetched, _ ) ->
                LT

            ( Waiting, NeverFetched ) ->
                GT

            ( Waiting, _ ) ->
                LT

            ( NextFetchAt _ _, NeverFetched ) ->
                GT

            ( NextFetchAt _ _, Waiting ) ->
                GT

            ( NextFetchAt p1 _, NextFetchAt p2 _ ) ->
                -- For ease of impl., not comparing current backoff
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( NextFetchAt _ _, _ ) ->
                LT

            ( InitialFetching, NeverFetched ) ->
                GT

            ( InitialFetching, Waiting ) ->
                GT

            ( InitialFetching, NextFetchAt _ _ ) ->
                GT

            ( InitialFetching, _ ) ->
                LT

            ( ResumeFetching, Fetching _ _ ) ->
                LT

            ( ResumeFetching, Available ) ->
                LT

            ( ResumeFetching, Forbidden ) ->
                LT

            ( ResumeFetching, _ ) ->
                GT

            ( Fetching p1 _, Fetching p2 _ ) ->
                Basics.compare (Time.posixToMillis p1) (Time.posixToMillis p2)

            ( Fetching _ _, Available ) ->
                LT

            ( Fetching _ _, Forbidden ) ->
                LT

            ( Fetching _ _, _ ) ->
                GT

            ( Available, Forbidden ) ->
                LT

            ( Available, _ ) ->
                GT

            ( Forbidden, _ ) ->
                GT


{-| True if the second one is less than the first one (pipeline-friendly order).
-}
lessThan : FetchStatus -> FetchStatus -> Bool
lessThan a b =
    compare b a == LT


isActive : FetchStatus -> Bool
isActive fetchStatus =
    case fetchStatus of
        NeverFetched ->
            False

        Waiting ->
            True

        NextFetchAt _ _ ->
            True

        InitialFetching ->
            False

        ResumeFetching ->
            True

        Fetching _ _ ->
            True

        Available ->
            False

        Forbidden ->
            False


isAvailable : FetchStatus -> Bool
isAvailable fetchStatus =
    fetchStatus == Available || isActive fetchStatus
