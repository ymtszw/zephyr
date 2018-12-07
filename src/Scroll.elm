module Scroll exposing
    ( Scroll, InitOptions, encode, decoder, init, initWith, defaultOptions, clear
    , setLimit
    , push, prependList, pop, toList, toListWithFilter, size, pendingSize, isEmpty, scrolled
    , Msg(..), update, scrollAttrs
    )

{-| Infinite-scroll-aware bounded List, backed by BoundedDeque.
Think the word "Scroll" as a noun, not a verb.

  - Tracks current status of users' viewport
  - Tracks current "scroll amount" by a concept of `Tier`
  - `toList` returns a certain amount of elements, based on its current `Tier`
      - If scrolled to higher Tiers, it returns more elements.
        If not scrolled at all, it returns minimum amount of them.
  - Has a hard limit on number of elements. Elements are evicted in FIFO manner.

It is a data structure AND a component. It behaves mostly like a pure data structure,
but changes its behavior based on runtime status, and achieves side-effect via component pattern (Msg and update).

Its ever-changing runtime statuses are ephemeral, and not persisted.
Its internal data structure may be persisted.

@docs Scroll, InitOptions, AutoAdjustOptions, encode, decoder, init, initWith, defaultOptions, clear
@docs setLimit
@docs push, prependList, pop, toList, toListWithFilter, size, pendingSize, isEmpty, scrolled
@docs Msg, update, scrollAttrs

-}

import BoundedDeque exposing (BoundedDeque)
import Browser.Dom
import Extra exposing (doAfter)
import Html
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Task


type Scroll a
    = Scroll (ScrollRecord a)


type alias ScrollRecord a =
    { id : String
    , buffer : BoundedDeque a
    , pending : List a
    , pendingSize : Int
    , lastBoundingHeight : Int
    , viewportStatus : ViewportStatus
    , tier : Tier
    , baseAmount : Int
    , tierAmount : Int
    , config : Config
    }


type ViewportStatus
    = Scrolling Browser.Dom.Viewport
    | OffTheTop Browser.Dom.Viewport
    | AtTop Browser.Dom.Viewport
    | Initial


type Tier
    = Tier Int


type alias Config =
    { baseRatio : Float
    , tierRatio : Float
    , ascendThreshold : Float
    , minimumItemHeight : Float
    }


encode : (a -> E.Value) -> Scroll a -> E.Value
encode encodeItem (Scroll s) =
    E.list encodeItem (BoundedDeque.toList s.buffer)


type alias InitOptions =
    { id : String
    , limit : Int
    , boundingHeight : Int
    , minimumItemHeight : Int
    , baseRatio : Float
    , tierRatio : Float
    , ascendThreshold : Float
    }


decoder : InitOptions -> Decoder a -> Decoder ( Scroll a, Cmd Msg )
decoder opts itemDecoder =
    D.do (D.list itemDecoder) <|
        \list ->
            -- Immediately requestAdjust, retrieving scene height from actually rendered node
            D.succeed ( Scroll (initImpl opts list), requestAdjust opts.id opts.boundingHeight )


ratioToAmount : Float -> Float -> Int
ratioToAmount fillAmountF ratio =
    round (fillAmountF * ratio)


requestAdjust : String -> Int -> Cmd Msg
requestAdjust id boundingHeight =
    Task.attempt
        (\res ->
            case res of
                Ok vp ->
                    AdjustExec boundingHeight vp

                Err e ->
                    ViewportResult (Err e)
        )
        (Browser.Dom.getViewportOf id)


{-| Initialize a Scroll with set of InitOptions.

To change options after initialized, use set\*\*\* functions.

-}
init : InitOptions -> Scroll a
init opts =
    Scroll (initImpl opts [])


initImpl : InitOptions -> List a -> ScrollRecord a
initImpl opts list =
    let
        minimumItemHeightF =
            toFloat opts.minimumItemHeight

        fillAmountF =
            toFloat opts.boundingHeight / minimumItemHeightF
    in
    { id = opts.id
    , buffer = BoundedDeque.fromList opts.limit list
    , pending = []
    , pendingSize = 0
    , lastBoundingHeight = opts.boundingHeight
    , viewportStatus = Initial
    , tier = Tier 0
    , baseAmount = ratioToAmount fillAmountF opts.baseRatio
    , tierAmount = ratioToAmount fillAmountF opts.tierRatio
    , config =
        { baseRatio = opts.baseRatio
        , tierRatio = opts.tierRatio
        , ascendThreshold = opts.ascendThreshold
        , minimumItemHeight = minimumItemHeightF
        }
    }


defaultOptions : { id : String, boundingHeight : Int, minimumItemHeight : Int } -> InitOptions
defaultOptions opts =
    { id = opts.id
    , limit = defaultLimit
    , boundingHeight = opts.boundingHeight
    , minimumItemHeight = opts.minimumItemHeight
    , baseRatio = defaultBaseRatio
    , tierRatio = defaultTierRatio
    , ascendThreshold = defaultAscendThreshold
    }


defaultLimit : Int
defaultLimit =
    1000


defaultBaseRatio : Float
defaultBaseRatio =
    1.5


defaultTierRatio : Float
defaultTierRatio =
    defaultBaseRatio * 2


defaultAscendThreshold : Float
defaultAscendThreshold =
    0.8


setLimit : Int -> Scroll a -> Scroll a
setLimit limit (Scroll s) =
    Scroll { s | buffer = BoundedDeque.resize (\_ -> limit) s.buffer }


{-| Initialize a Scroll with InitOptions and initial items.

Order of initial items are kept as is: head at the front.

-}
initWith : InitOptions -> List a -> Scroll a
initWith opts list =
    Scroll (initImpl opts list)


{-| Clear exisitng elements (in `buffer` and `pending`) from a Scroll.
-}
clear : Scroll a -> Scroll a
clear (Scroll s) =
    Scroll { s | pending = [], buffer = BoundedDeque.empty (BoundedDeque.getMaxSize s.buffer) }



-- Pure APIs


{-| Push an element to a Scroll.

Internally the element is pushed to:

  - `buffer` if viewport is at the top
  - `pending` if viewport is not at the top

If pushed to `buffer`, `toList` and `toListWithFilter` caches are invalidated.

-}
push : a -> Scroll a -> Scroll a
push a (Scroll s) =
    case s.viewportStatus of
        Initial ->
            Scroll s |> pendingToBuffer |> pushToBuffer a

        AtTop _ ->
            Scroll s |> pendingToBuffer |> pushToBuffer a

        _ ->
            pushToPending a (Scroll s)


pendingToBuffer : Scroll a -> Scroll a
pendingToBuffer (Scroll s) =
    case s.pending of
        [] ->
            Scroll s

        nonEmpty ->
            Scroll { s | buffer = List.foldr BoundedDeque.pushFront s.buffer nonEmpty, pending = [], pendingSize = 0 }


pushToBuffer : a -> Scroll a -> Scroll a
pushToBuffer a (Scroll s) =
    Scroll { s | buffer = BoundedDeque.pushFront a s.buffer }


pushToPending : a -> Scroll a -> Scroll a
pushToPending a (Scroll s) =
    Scroll { s | pending = a :: s.pending, pendingSize = s.pendingSize + 1 }


{-| Prepend List of items to a Scroll. Consider as tail-first push.
-}
prependList : List a -> Scroll a -> Scroll a
prependList list (Scroll s) =
    case s.viewportStatus of
        Initial ->
            Scroll s |> pendingToBuffer |> prependToBuffer list

        AtTop _ ->
            Scroll s |> pendingToBuffer |> prependToBuffer list

        _ ->
            Scroll { s | pending = list ++ s.pending, pendingSize = s.pendingSize + List.length list }


prependToBuffer : List a -> Scroll a -> Scroll a
prependToBuffer list (Scroll s) =
    let
        limit =
            BoundedDeque.getMaxSize s.buffer
    in
    Scroll { s | buffer = BoundedDeque.append (BoundedDeque.fromList limit list) s.buffer }


{-| Pop an element from the front of a Scroll.

"The front" points to either `buffer` or `pending` internally,
depending on `viewportStatus`.

-}
pop : Scroll a -> ( Maybe a, Scroll a )
pop (Scroll s) =
    case s.viewportStatus of
        Initial ->
            Scroll s |> pendingToBuffer |> popFromBuffer

        AtTop _ ->
            Scroll s |> pendingToBuffer |> popFromBuffer

        _ ->
            popFromPending (Scroll s)


popFromBuffer : Scroll a -> ( Maybe a, Scroll a )
popFromBuffer (Scroll s) =
    BoundedDeque.popFront s.buffer
        |> Tuple.mapSecond
            (\buffer ->
                Scroll { s | buffer = buffer }
            )


popFromPending : Scroll a -> ( Maybe a, Scroll a )
popFromPending (Scroll s) =
    case s.pending of
        [] ->
            ( Nothing, Scroll s )

        x :: xs ->
            ( Just x, Scroll { s | pending = xs, pendingSize = s.pendingSize - 1 } )


{-| Take certain amount of elements from `buffer`.

The amount is calculated from viewportStatus and init options.

It does NOT move `pending` to `buffer`.

-}
toList : Scroll a -> List a
toList (Scroll s) =
    BoundedDeque.takeFront (amountToTake (Scroll s)) s.buffer


amountToTake : Scroll a -> Int
amountToTake (Scroll s) =
    let
        (Tier t) =
            s.tier
    in
    s.baseAmount + s.tierAmount * t


{-| `toList` with filtering.
-}
toListWithFilter : (a -> Bool) -> Scroll a -> List a
toListWithFilter check (Scroll s) =
    let
        amount =
            amountToTake (Scroll s)

        reducer a acc =
            if acc.size == amount then
                acc

            else if check a then
                { acc | list = a :: acc.list, size = acc.size + 1 }

            else
                acc
    in
    BoundedDeque.foldl reducer { list = [], size = 0 } s.buffer
        |> (\{ list } -> List.reverse list)


{-| Number of recorded items. It returns cached values so works in _O(1)_.
-}
size : Scroll a -> Int
size (Scroll s) =
    -- BoundedDeque.length uses cached List.length (for triggering rebalance) so works in _O(1)_
    BoundedDeque.length s.buffer + s.pendingSize


{-| Cached length of `pending` List. Takes _O(1)_.

As long as elements are pushed/popped via exposed APIs, this value should be accurate

-}
pendingSize : Scroll a -> Int
pendingSize (Scroll s) =
    s.pendingSize


isEmpty : Scroll a -> Bool
isEmpty (Scroll s) =
    BoundedDeque.isEmpty s.buffer


scrolled : Scroll a -> Bool
scrolled (Scroll s) =
    case s.viewportStatus of
        Initial ->
            False

        AtTop _ ->
            False

        OffTheTop _ ->
            True

        Scrolling _ ->
            True



-- Side-effect APIs


type Msg
    = ScrollStart
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BackToTop
    | Reveal
    | NewItem
    | LoadMore
    | AdjustReq Int
    | AdjustExec Int Browser.Dom.Viewport


update : Msg -> Scroll a -> ( Scroll a, Cmd Msg )
update msg (Scroll s) =
    case msg of
        ScrollStart ->
            case s.viewportStatus of
                OffTheTop vp ->
                    ( Scroll { s | viewportStatus = Scrolling vp }, queryViewportWithDelay s.id )

                AtTop vp ->
                    ( Scroll { s | viewportStatus = Scrolling vp }, queryViewportWithDelay s.id )

                Initial ->
                    ( Scroll s, queryViewportWithDelay s.id )

                Scrolling _ ->
                    ( Scroll s, Cmd.none )

        ViewportResult (Ok newVp) ->
            if newVp.viewport.y == 0 then
                ( Scroll { s | viewportStatus = AtTop newVp } |> pendingToBuffer |> calculateTier, Cmd.none )

            else
                case s.viewportStatus of
                    Scrolling oldVp ->
                        if newVp == oldVp then
                            ( Scroll { s | viewportStatus = OffTheTop newVp } |> calculateTier, Cmd.none )

                        else
                            ( Scroll { s | viewportStatus = Scrolling newVp }, queryViewportWithDelay s.id )

                    _ ->
                        ( Scroll { s | viewportStatus = OffTheTop newVp } |> calculateTier, Cmd.none )

        ViewportResult (Err (Browser.Dom.NotFound _)) ->
            -- Column is dismissed? Keep current state
            ( Scroll s, Cmd.none )

        BackToTop ->
            -- Lazily resolves viewportStatus, not touching Scroll.
            -- In my experience, this achieves the most consistent and acceptable behavior
            ( Scroll s
            , Task.map2 (\() vpRes -> vpRes)
                (Browser.Dom.setViewportOf s.id 0 0)
                (Browser.Dom.getViewportOf s.id)
                |> Task.attempt ViewportResult
            )

        Reveal ->
            ( Scroll { s | viewportStatus = Initial } |> pendingToBuffer |> calculateTier
            , queryViewportWithDelay s.id
            )

        NewItem ->
            ( Scroll s, requestAdjust s.id s.lastBoundingHeight )

        LoadMore ->
            -- LoadMore is manually requested when auto adjusting is somewhat stopped/caught up
            -- Force adjusting and incrementing Tier
            ( incrementTier (Scroll s), requestAdjust s.id s.lastBoundingHeight )

        AdjustReq boundingHeight ->
            if boundingHeight /= s.lastBoundingHeight then
                ( Scroll s, requestAdjust s.id boundingHeight )

            else
                ( Scroll s, Cmd.none )

        AdjustExec boundingHeight vp ->
            -- Adjust parameters dynamically (in somewhat crude way),
            -- by comparing actual node height to current boundingHeight (usually a clientHeight for full-height node).
            let
                approxAverageItemHeightF =
                    -- This value is no more than approximate, due to item grouping and filtering
                    vp.scene.height / toFloat (amountToTake (Scroll s))

                approxFillAmountF =
                    toFloat boundingHeight / approxAverageItemHeightF
            in
            ( Scroll
                { s
                    | viewportStatus = AtTop vp
                    , lastBoundingHeight = boundingHeight
                    , baseAmount = ratioToAmount approxFillAmountF s.config.baseRatio
                    , tierAmount = ratioToAmount approxFillAmountF s.config.tierRatio
                }
            , queryViewportWithDelay s.id
            )


queryViewportWithDelay : String -> Cmd Msg
queryViewportWithDelay id =
    doAfter queryDelay (Result.map Tuple.second >> ViewportResult) (Browser.Dom.getViewportOf id)


queryDelay : Float
queryDelay =
    250


calculateTier : Scroll a -> Scroll a
calculateTier (Scroll s) =
    case s.viewportStatus of
        Initial ->
            Scroll { s | tier = Tier 0 }

        AtTop _ ->
            Scroll { s | tier = Tier 0 }

        OffTheTop vp ->
            calculateTierImpl vp (Scroll s)

        Scrolling vp ->
            calculateTierImpl vp (Scroll s)


calculateTierImpl : Browser.Dom.Viewport -> Scroll s -> Scroll s
calculateTierImpl vp (Scroll s) =
    let
        viewportBottom =
            vp.viewport.y + vp.viewport.height
    in
    if viewportBottom / vp.scene.height >= s.config.ascendThreshold then
        incrementTier (Scroll s)

    else
        Scroll s


incrementTier : Scroll s -> Scroll s
incrementTier (Scroll s) =
    let
        (Tier n) =
            s.tier
    in
    Scroll { s | tier = Tier (n + 1) }


scrollAttrs : (Msg -> msg) -> Scroll a -> List (Html.Attribute msg)
scrollAttrs tagger (Scroll s) =
    case s.viewportStatus of
        Initial ->
            [ id s.id, Html.Events.on "scroll" (D.succeed (tagger ScrollStart)) ]

        AtTop _ ->
            [ id s.id, Html.Events.on "scroll" (D.succeed (tagger ScrollStart)) ]

        OffTheTop _ ->
            [ id s.id, Html.Events.on "scroll" (D.succeed (tagger ScrollStart)) ]

        Scrolling _ ->
            [ id s.id ]
