module Logger exposing (Entry, History, historyEl, init, rec)

import BoundedDeque exposing (BoundedDeque)
import Data.ColorTheme exposing (oneDark)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Element.Input
import Html.Attributes exposing (readonly, style, tabindex)
import View.Parts exposing (scale12)


type History
    = History (BoundedDeque Entry)


type alias Entry =
    { ctor : String
    , payload : List String
    }


init : History
init =
    History (BoundedDeque.empty historyLimit)


historyLimit : Int
historyLimit =
    1000


rec : History -> Entry -> History
rec (History q) e =
    History <|
        case BoundedDeque.popFront q of
            ( Just h, popped ) ->
                if h.ctor == e.ctor then
                    BoundedDeque.pushFront e popped

                else
                    BoundedDeque.pushFront e q

            ( Nothing, _ ) ->
                BoundedDeque.pushFront e q


historyEl : History -> Element Msg
historyEl (History history) =
    table
        [ width fill
        , height (shrink |> maximum 400)
        , padding 5
        , spacing 2
        , clipX
        , BG.color oneDark.main
        ]
        { data = BoundedDeque.toList history
        , columns = [ ctorColumnEl, payloadColumnEl ]
        }
        |> el
            [ width fill
            , padding 10
            , BD.rounded 5
            , BG.color oneDark.sub
            , Font.size (scale12 1)
            ]


ctorColumnEl : Column Entry Msg
ctorColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Msg"
    , width = fill
    , view = \entry -> el [ bold ] (text entry.ctor)
    }


payloadColumnEl : Column Entry Msg
payloadColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Payload"
    , width = fill
    , view =
        \entry ->
            column [ width fill ] (List.map preEl entry.payload)
    }


preEl : String -> Element Msg
preEl raw =
    Element.Input.multiline
        [ width fill
        , height shrink
        , padding 0
        , BG.color oneDark.bg
        , BD.width 0
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
