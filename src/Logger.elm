module Logger exposing (Entry, History, historyEl, init, rec)

import Data.ColorTheme exposing (oneDark)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Html.Attributes exposing (style)
import Ring exposing (Ring(..))
import View.Parts exposing (scale12)


type History
    = History (Ring Entry)


type alias Entry =
    { ctor : String
    , payload : List String
    }


init : History
init =
    History (Ring.init historyLimit)


historyLimit : Int
historyLimit =
    1000


rec : History -> Entry -> History
rec (History r) e =
    History <| Ring.consIf (\e1 e2 -> e1.ctor /= e2.ctor) e r


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
        { data = Ring.toList history
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
