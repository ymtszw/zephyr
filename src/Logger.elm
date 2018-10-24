module Logger exposing (Entry, History, Msg(..), historyEl, init, rec, update)

import BoundedDeque exposing (BoundedDeque)
import Browser.Dom exposing (Viewport, getViewportOf, setViewportOf)
import Data.ColorTheme exposing (oneDark)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Element.Input
import Extra exposing (doAfter)
import Html.Attributes exposing (id, readonly, style, tabindex)
import Html.Events
import Json.Decode as D
import Task
import Time exposing (Posix)
import View.Parts exposing (noneAttr, scale12)


type History
    = History
        { buffer : BoundedDeque Entry
        , pending : List Entry
        , scroll : Scroll
        }


type alias Entry =
    { ctor : String
    , payload : List String
    }


type Scroll
    = Scrolling Viewport
    | OffTheTop Viewport
    | AtTop


type Msg
    = ScrollStart
    | ViewportResult (Result Browser.Dom.Error ( Posix, Viewport ))
    | BackToTop
    | NoOp


init : History
init =
    History
        { buffer = BoundedDeque.empty historyLimit
        , pending = []
        , scroll = AtTop
        }


historyLimit : Int
historyLimit =
    1000


update : Msg -> History -> ( History, Cmd Msg )
update msg (History h) =
    case ( msg, h.scroll ) of
        ( ScrollStart, OffTheTop vp ) ->
            ( History { h | scroll = Scrolling vp }, queryViewport )

        ( ScrollStart, _ ) ->
            ( History h, queryViewport )

        ( BackToTop, _ ) ->
            ( History h, setViewportOf historyElementId 0 0 |> Task.attempt (\_ -> ScrollStart) )

        ( ViewportResult (Ok ( _, newVp )), Scrolling oldVp ) ->
            if newVp.viewport.y == 0 then
                ( History { h | scroll = AtTop } |> pendingToBuffer, Cmd.none )

            else if newVp == oldVp then
                ( History { h | scroll = OffTheTop newVp }, Cmd.none )

            else
                ( History { h | scroll = Scrolling newVp }, queryViewport )

        ( ViewportResult (Ok ( _, newVp )), _ ) ->
            if newVp.viewport.y == 0 then
                ( History { h | scroll = AtTop } |> pendingToBuffer, Cmd.none )

            else
                ( History { h | scroll = OffTheTop newVp }, Cmd.none )

        ( ViewportResult (Err _), _ ) ->
            ( History { h | scroll = AtTop }, Cmd.none )

        ( NoOp, _ ) ->
            ( History h, Cmd.none )


queryViewport : Cmd Msg
queryViewport =
    doAfter 50 ViewportResult (getViewportOf historyElementId)


pendingToBuffer : History -> History
pendingToBuffer (History h) =
    History <|
        { h
            | buffer = List.foldr BoundedDeque.pushFront h.buffer h.pending
            , pending = []
        }


rec : History -> Entry -> History
rec (History h) e =
    case h.scroll of
        AtTop ->
            pendingToBuffer (History h) |> pushToBuffer e

        _ ->
            pushToPending e (History h)


pushToBuffer : Entry -> History -> History
pushToBuffer e (History h) =
    History <|
        case BoundedDeque.popFront h.buffer of
            ( Just top, popped ) ->
                if top.ctor == e.ctor then
                    { h | buffer = BoundedDeque.pushFront e popped }

                else
                    { h | buffer = BoundedDeque.pushFront e h.buffer }

            ( Nothing, _ ) ->
                { h | buffer = BoundedDeque.pushFront e h.buffer }


pushToPending : Entry -> History -> History
pushToPending e (History h) =
    History <|
        case h.pending of
            [] ->
                { h | pending = [ e ] }

            p :: ps ->
                if p.ctor == e.ctor then
                    { h | pending = e :: ps }

                else
                    { h | pending = e :: h.pending }


historyEl : History -> Element Msg
historyEl (History h) =
    table
        [ width fill
        , height (shrink |> maximum 400)
        , padding 5
        , spacing 2
        , clipX
        , BG.color oneDark.main
        , htmlAttribute (id historyElementId)
        , detectScroll (History h)
        ]
        { data = BoundedDeque.toList h.buffer
        , columns = [ ctorColumnEl, payloadColumnEl ]
        }
        |> el
            [ width fill
            , padding 10
            , BD.rounded 5
            , BG.color oneDark.sub
            , Font.size (scale12 1)
            , inFront (newEntryEl (History h))
            ]


historyElementId : String
historyElementId =
    "loggerHistory"


detectScroll : History -> Element.Attribute Msg
detectScroll (History h) =
    case h.scroll of
        AtTop ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        OffTheTop _ ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        Scrolling _ ->
            noneAttr


newEntryEl : History -> Element Msg
newEntryEl (History h) =
    case h.pending of
        [] ->
            none

        ps ->
            el [ width fill, alignTop, padding 10 ] <|
                Element.Input.button [ width fill, padding 5, BG.color oneDark.succ ]
                    { onPress = Just BackToTop
                    , label = el [ centerX ] <| text ("New Log Entry (" ++ String.fromInt (List.length ps) ++ ")")
                    }


ctorColumnEl : Column Entry Msg
ctorColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Msg"
    , width = fill
    , view = \entry -> el [ bold ] (text entry.ctor)
    }


payloadColumnEl : Column Entry Msg
payloadColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Payload"
    , width = fillPortion 2
    , view =
        \entry ->
            column [ width fill, spacing 2 ] (List.map payloadEl entry.payload)
    }


payloadEl : String -> Element Msg
payloadEl raw =
    el [ width fill, padding 5, BG.color oneDark.bg ] <|
        Element.Input.multiline
            [ width fill
            , height (shrink |> minimum 16 |> maximum 100)
            , padding 0
            , focused []
            , BD.width 0
            , BG.color oneDark.bg
            , Font.family [ Font.typeface "consolas", Font.monospace ]
            , htmlAttribute (readonly True)
            , htmlAttribute (tabindex -1)
            ]
            { onChange = always NoOp
            , text = raw
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Payload"
            , spellcheck = False
            }
