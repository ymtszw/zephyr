module Data.Pref exposing (Pref, adjustEvictThreashold, decoder, encode, init, storeId, update)

import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import View.Parts exposing (fixedColumnWidth)


{-| In "Zephyr mode", Columns are automatically evicted (dismissed)
and reappear when new messages arrived.

`evictThreshold` dictates how many columns can be displayed at a time, in Zephyr mode.
This value is automatically adjusted according to clientWidth.

-}
type alias Pref =
    { zephyrMode : Bool
    , evictThreshold : Int
    }


init : Int -> Pref
init clientWidth =
    { zephyrMode = True
    , evictThreshold = adjustEvictThreashold clientWidth
    }


adjustEvictThreashold : Int -> Int
adjustEvictThreashold clientWidth =
    (clientWidth // fixedColumnWidth) + 2


encode : Pref -> Storable
encode pref =
    Data.Storable.encode storeId
        [ ( "zephyrMode", E.bool pref.zephyrMode )
        ]


storeId : String
storeId =
    "pref"


decoder : Int -> Decoder Pref
decoder clientWidth =
    D.oneOf
        [ D.map2 Pref
            (D.field "zephyrMode" D.bool)
            (D.succeed (adjustEvictThreashold clientWidth))
        , D.succeed (init clientWidth) -- Casually provide the default, rather than fail on Pref load
        ]


update : Bool -> Pref -> ( Pref, Bool )
update zephyrMode pref =
    if pref.zephyrMode == zephyrMode then
        ( pref, False )

    else
        ( { pref | zephyrMode = zephyrMode }, True )
