module Scroll exposing
    ( Scroll, Options, encode, decoder, init, initWith, defaultOptions, clear
    , setLimit, setBaseAmount, setTierAmount, setAscendThreshold
    , push, pushAll, pop, toList, toListWithFilter, pendingSize, isEmpty, scrolled
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

@docs Scroll, Options, encode, decoder, init, initWith, defaultOptions, clear
@docs setLimit, setBaseAmount, setTierAmount, setAscendThreshold
@docs push, pushAll, pop, toList, toListWithFilter, pendingSize, isEmpty, scrolled
@docs Msg, update, scrollAttrs

-}

import BoundedDeque exposing (BoundedDeque)
import Browser.Dom
import Extra exposing (doAfter)
import Html
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Task


type Scroll a
    = Scroll (ScrollRecord a)


type alias ScrollRecord a =
    { id : String
    , buffer : BoundedDeque a
    , pending : List a
    , pendingSize : Int
    , viewportStatus : ViewportStatus
    , tier : Tier
    , baseAmount : Int
    , tierAmount : Int
    , ascendThreshold : Float
    }


type ViewportStatus
    = Scrolling Browser.Dom.Viewport
    | OffTheTop Browser.Dom.Viewport
    | AtTop


type Tier
    = Tier Int


encode : (a -> E.Value) -> Scroll a -> E.Value
encode encodeItem (Scroll s) =
    E.list encodeItem (BoundedDeque.toList s.buffer)


decoder : Options -> Decoder a -> Decoder (Scroll a)
decoder { id, limit, baseAmount, tierAmount, ascendThreshold } itemDecoder =
    D.list itemDecoder
        |> D.map
            (\list ->
                Scroll
                    { id = id
                    , buffer = BoundedDeque.fromList limit list
                    , pending = []
                    , pendingSize = 0
                    , viewportStatus = AtTop
                    , tier = Tier 0
                    , baseAmount = baseAmount
                    , tierAmount = tierAmount
                    , ascendThreshold = ascendThreshold
                    }
            )


type alias Options =
    { id : String, limit : Int, baseAmount : Int, tierAmount : Int, ascendThreshold : Float }


{-| Initialize a Scroll with set of Options.

To change options after initialized, use set\*\*\* functions.

-}
init : Options -> Scroll a
init { id, limit, baseAmount, tierAmount, ascendThreshold } =
    Scroll
        { id = id
        , buffer = BoundedDeque.empty limit
        , pending = []
        , pendingSize = 0
        , viewportStatus = AtTop
        , tier = Tier 0
        , baseAmount = baseAmount
        , tierAmount = tierAmount
        , ascendThreshold = ascendThreshold
        }


defaultOptions : String -> Options
defaultOptions id =
    { id = id
    , limit = defaultLimit
    , baseAmount = defaultBaseAmount
    , tierAmount = defaultTierAmount
    , ascendThreshold = defaultAscendThreshold
    }


defaultLimit : Int
defaultLimit =
    1000


defaultBaseAmount : Int
defaultBaseAmount =
    20


defaultTierAmount : Int
defaultTierAmount =
    defaultBaseAmount


defaultAscendThreshold : Float
defaultAscendThreshold =
    0.8


setLimit : Int -> Scroll a -> Scroll a
setLimit limit (Scroll s) =
    Scroll { s | buffer = BoundedDeque.resize (\_ -> limit) s.buffer }


setBaseAmount : Int -> Scroll a -> Scroll a
setBaseAmount baseAmount (Scroll s) =
    Scroll { s | baseAmount = baseAmount }


setTierAmount : Int -> Scroll a -> Scroll a
setTierAmount tierAmount (Scroll s) =
    Scroll { s | tierAmount = tierAmount }


setAscendThreshold : Float -> Scroll a -> Scroll a
setAscendThreshold ascendThreshold (Scroll s) =
    Scroll { s | ascendThreshold = ascendThreshold }


{-| Initialize a Scroll with Options and initial items.

Order of initial items are kept as is: head at the front.

-}
initWith : Options -> List a -> Scroll a
initWith { id, limit, baseAmount, tierAmount, ascendThreshold } list =
    Scroll
        { id = id
        , buffer = BoundedDeque.fromList limit list
        , pending = []
        , pendingSize = 0
        , viewportStatus = AtTop
        , tier = Tier 0
        , baseAmount = baseAmount
        , tierAmount = tierAmount
        , ascendThreshold = ascendThreshold
        }


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
        AtTop ->
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


{-| Push all items in a List to a Scroll. Head-first.
-}
pushAll : List a -> Scroll a -> Scroll a
pushAll list s =
    case list of
        [] ->
            s

        x :: xs ->
            pushAll xs (push x s)


{-| Pop an element from the front of a Scroll.

"The front" points to either `buffer` or `pending` internally,
depending on `viewportStatus`.

-}
pop : Scroll a -> ( Maybe a, Scroll a )
pop (Scroll s) =
    case s.viewportStatus of
        AtTop ->
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
    s.baseAmount + s.tierAmount * (t + 1)


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
        AtTop ->
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


update : Msg -> Scroll a -> ( Scroll a, Cmd Msg )
update msg (Scroll s) =
    case msg of
        ScrollStart ->
            case s.viewportStatus of
                OffTheTop vp ->
                    ( Scroll { s | viewportStatus = Scrolling vp }, queryViewport s.id )

                AtTop ->
                    ( Scroll s, queryViewport s.id )

                Scrolling _ ->
                    ( Scroll s, Cmd.none )

        ViewportResult (Ok newVp) ->
            if newVp.viewport.y == 0 then
                ( Scroll { s | viewportStatus = AtTop } |> pendingToBuffer |> calculateTier, Cmd.none )

            else
                case s.viewportStatus of
                    Scrolling oldVp ->
                        if newVp == oldVp then
                            ( Scroll { s | viewportStatus = OffTheTop newVp } |> calculateTier, Cmd.none )

                        else
                            ( Scroll { s | viewportStatus = Scrolling newVp } |> calculateTier, queryViewport s.id )

                    _ ->
                        ( Scroll { s | viewportStatus = OffTheTop newVp } |> calculateTier, Cmd.none )

        ViewportResult (Err _) ->
            ( Scroll { s | viewportStatus = AtTop } |> pendingToBuffer |> calculateTier, Cmd.none )

        BackToTop ->
            -- Lazily resolves viewportStatus, not touching Scroll.
            -- In my experience, this achieves the most consistent and acceptable behavior
            ( Scroll s
            , Task.map2 (\() vpRes -> vpRes)
                (Browser.Dom.setViewportOf s.id 0 0)
                (Browser.Dom.getViewportOf s.id)
                |> Task.attempt ViewportResult
            )


queryViewport : String -> Cmd Msg
queryViewport id =
    doAfter queryInterval (Result.map Tuple.second >> ViewportResult) (Browser.Dom.getViewportOf id)


queryInterval : Float
queryInterval =
    150


calculateTier : Scroll a -> Scroll a
calculateTier (Scroll s) =
    case s.viewportStatus of
        AtTop ->
            Scroll { s | tier = Tier 0 }

        OffTheTop vp ->
            calculateTierImpl vp (Scroll s)

        Scrolling vp ->
            calculateTierImpl vp (Scroll s)


calculateTierImpl : Browser.Dom.Viewport -> Scroll s -> Scroll s
calculateTierImpl vp (Scroll s) =
    if (vp.viewport.y + vp.viewport.height) / vp.scene.height >= s.ascendThreshold then
        let
            (Tier n) =
                s.tier
        in
        Scroll { s | tier = Tier (n + 1) }

    else
        Scroll s


scrollAttrs : (Msg -> msg) -> Scroll a -> List (Html.Attribute msg)
scrollAttrs tagger (Scroll s) =
    case s.viewportStatus of
        AtTop ->
            [ id s.id, Html.Events.on "scroll" (D.succeed (tagger ScrollStart)) ]

        OffTheTop _ ->
            [ id s.id, Html.Events.on "scroll" (D.succeed (tagger ScrollStart)) ]

        Scrolling _ ->
            [ id s.id ]
