module Logger exposing (Entry, History, historyEl, init, rec)

import BoundedDeque exposing (BoundedDeque)
import Data.ColorTheme exposing (oneDark)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Html.Attributes exposing (style)
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


historyEl : History -> Element msg
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


ctorColumnEl : Column Entry msg
ctorColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Msg"
    , width = fill
    , view = \entry -> el [ bold ] (text entry.ctor)
    }


payloadColumnEl : Column Entry msg
payloadColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Payload"
    , width = fill
    , view =
        \entry ->
            column [ width fill ] (List.map preEl entry.payload)
    }


preEl : String -> Element msg
preEl raw =
    paragraph
        [ width fill
        , BG.color oneDark.bg
        , Font.family [ Font.typeface "consolas", Font.monospace ]
        ]
        [ text raw ]
