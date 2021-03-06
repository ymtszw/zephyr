module Data.Pref exposing (Msg(..), Pref, adjustEvictThreashold, decoder, encode, init, storeId, update)

import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import View.Templates.Main exposing (columnWidth)


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
    max 2 (clientWidth // columnWidth)


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


type Msg
    = ZephyrMode Bool


update : Msg -> Pref -> ( Pref, Bool )
update msg pref =
    let
        saveOrPure prev new updater =
            if prev == new then
                ( pref, False )

            else
                ( updater new, True )
    in
    case msg of
        ZephyrMode bool ->
            saveOrPure pref.zephyrMode bool <| \new -> { pref | zephyrMode = new }
