module Scroll exposing
    ( Scroll, InitOptions, encode, decoder, init, initWith, defaultOptions, clear
    , setLimit
    , push, prependList, pop, getAt, toList, toListWithFilter, size, pendingSize, isEmpty, scrolled
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
@docs push, prependList, pop, getAt, toList, toListWithFilter, size, pendingSize, isEmpty, scrolled
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
import List.Extra
import Process
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
    = Initial
    | AtTop Browser.Dom.Viewport
    | OffTheTop Browser.Dom.Viewport
    | ReturningToTop Browser.Dom.Viewport


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
            -- Immediately adjustParams, retrieving scene height from actually rendered node
            D.succeed ( Scroll (initImpl opts list), adjustParams opts.id opts.boundingHeight )


adjustParams : String -> Int -> Cmd Msg
adjustParams id boundingHeight =
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


ratioToAmount : Float -> Float -> Int
ratioToAmount fillAmountF ratio =
    round (fillAmountF * ratio)


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


getAt : Int -> Scroll a -> Maybe a
getAt index (Scroll s) =
    if 0 <= index && index < BoundedDeque.length s.buffer then
        List.Extra.getAt index (BoundedDeque.toList s.buffer)

    else
        Nothing


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
    max minAmountToTake (s.baseAmount + s.tierAmount * t)


{-| If there are large disparity in column items' heights,
amountToTake may get too small compared to boundingHeight.
Therefore this lowerbound.

The problematic situation can happen more frequently when boundingHeight is relatively low.

-}
minAmountToTake : Int
minAmountToTake =
    10


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

        ReturningToTop _ ->
            True



-- Side-effect APIs


type Msg
    = Scrolled Browser.Dom.Viewport
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BackToTop
    | RequestNextAnimationStep Float
    | Reveal
    | NewItem
    | LoadMore
    | AdjustReq Int
    | AdjustExec Int Browser.Dom.Viewport


update : Msg -> Scroll a -> ( Scroll a, Cmd Msg )
update msg (Scroll s) =
    case msg of
        Scrolled vp ->
            ( setViewportStatus vp (Scroll s), Cmd.none )

        ViewportResult (Ok vp) ->
            ( setViewportStatus vp (Scroll s), Cmd.none )

        ViewportResult (Err (Browser.Dom.NotFound _)) ->
            -- Column is dismissed? Keep current state
            ( Scroll s, Cmd.none )

        BackToTop ->
            -- Manually animated scroll. After completion, lazily resolves viewportStatus.
            -- In my experience, this achieves the most consistent and acceptable behavior
            case s.viewportStatus of
                OffTheTop vp ->
                    ( Scroll { s | viewportStatus = ReturningToTop vp }
                    , scrollWithManualAnimation s.id vp 0
                    )

                _ ->
                    -- Otherwise BackToTop does not make sense
                    ( Scroll s, Cmd.none )

        RequestNextAnimationStep currentStep ->
            case s.viewportStatus of
                ReturningToTop vp ->
                    ( Scroll s, scrollWithManualAnimation s.id vp currentStep )

                _ ->
                    -- Otherwise request should not arrive; discard.
                    ( Scroll s, Cmd.none )

        Reveal ->
            ( Scroll { s | viewportStatus = Initial } |> pendingToBuffer |> calculateTier
            , Task.attempt ViewportResult (Browser.Dom.getViewportOf s.id)
            )

        NewItem ->
            ( Scroll s, adjustParams s.id s.lastBoundingHeight )

        LoadMore ->
            -- LoadMore is manually requested when auto adjusting is somewhat stopped/caught up
            -- NOT adjusting params here since depending on contents,
            -- it can go in circle of adjust => scan => adjust again loop
            ( incrementTier (Scroll s), Cmd.none )

        AdjustReq boundingHeight ->
            if boundingHeight /= s.lastBoundingHeight then
                ( Scroll s, adjustParams s.id boundingHeight )

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
                    | lastBoundingHeight = boundingHeight
                    , baseAmount = ratioToAmount approxFillAmountF s.config.baseRatio
                    , tierAmount = ratioToAmount approxFillAmountF s.config.tierRatio
                }
                |> setViewportStatus vp
            , Cmd.none
            )


setViewportStatus : Browser.Dom.Viewport -> Scroll a -> Scroll a
setViewportStatus vp (Scroll s) =
    if vp.viewport.y == 0 then
        -- Scrolled to the top, either manually or via auto-travel
        Scroll { s | viewportStatus = AtTop vp } |> pendingToBuffer |> calculateTier

    else
        case s.viewportStatus of
            ReturningToTop _ ->
                -- Do not interfare return-travel with manual scroll
                Scroll s

            _ ->
                Scroll { s | viewportStatus = OffTheTop vp } |> calculateTier


calculateTier : Scroll a -> Scroll a
calculateTier (Scroll s) =
    case s.viewportStatus of
        Initial ->
            Scroll { s | tier = Tier 0 }

        AtTop _ ->
            Scroll { s | tier = Tier 0 }

        OffTheTop vp ->
            calculateTierImpl vp (Scroll s)

        ReturningToTop _ ->
            -- Do not change Tier while traveling top
            Scroll s


calculateTierImpl : Browser.Dom.Viewport -> Scroll s -> Scroll s
calculateTierImpl vp (Scroll s) =
    let
        viewportBottom =
            vp.viewport.y + vp.viewport.height
    in
    if viewportBottom >= vp.scene.height then
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


scrollWithManualAnimation : String -> Browser.Dom.Viewport -> Float -> Cmd Msg
scrollWithManualAnimation id originalVp step =
    let
        nextStep =
            step + 1
    in
    if nextStep > numberOfAnimationStep then
        Task.attempt ViewportResult (Browser.Dom.getViewportOf id)

    else
        let
            nextViewportY =
                originalVp.viewport.y * easeOutCubic (nextStep / numberOfAnimationStep)

            requestNext result =
                case result of
                    Ok () ->
                        RequestNextAnimationStep nextStep

                    Err domErr ->
                        ViewportResult (Err domErr)

            waitAFrame =
                -- It is terser to use this crude delaying rather than subscribing to animationFrame
                Process.sleep 1
        in
        waitAFrame
            |> Task.andThen (\() -> Browser.Dom.setViewportOf id 0 nextViewportY)
            |> Task.attempt requestNext


easeOutCubic : Float -> Float
easeOutCubic x =
    negate ((x - 1) ^ 3)


numberOfAnimationStep : Float
numberOfAnimationStep =
    25



-- VIEW API


scrollAttrs : (Msg -> msg) -> Scroll a -> List (Html.Attribute msg)
scrollAttrs tagger (Scroll s) =
    let
        scrollHandler =
            Html.Events.on "scroll" (D.map (tagger << Scrolled) domViewportDecoder)
    in
    case s.viewportStatus of
        Initial ->
            [ id s.id, scrollHandler ]

        AtTop _ ->
            [ id s.id, scrollHandler ]

        OffTheTop _ ->
            [ id s.id, scrollHandler ]

        ReturningToTop _ ->
            [ id s.id ]


domViewportDecoder : Decoder Browser.Dom.Viewport
domViewportDecoder =
    let
        sceneDecoder =
            D.map2 makeScene
                (D.field "scrollWidth" D.float)
                (D.field "scrollHeight" D.float)

        viewportDecoder =
            D.map4 makeViewport
                (D.field "scrollLeft" D.float)
                (D.field "scrollTop" D.float)
                (D.field "clientWidth" D.float)
                (D.field "clientHeight" D.float)

        makeScene w h =
            { width = w, height = h }

        makeViewport x y w h =
            { x = x, y = y, width = w, height = h }

        makeDomViewport s vp =
            { scene = s, viewport = vp }
    in
    D.field "target" (D.map2 makeDomViewport sceneDecoder viewportDecoder)
