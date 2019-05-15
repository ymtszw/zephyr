module View.Atoms.Cursor exposing (allScroll, default, pointer, styles, zoomIn)

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Style exposing (..)


default : Attribute msg
default =
    class defaultClass


pointer : Attribute msg
pointer =
    class pointerClass


allScroll : Attribute msg
allScroll =
    class allScrollClass


zoomIn : Attribute msg
zoomIn =
    class zoomInClass


styles : List Style
styles =
    [ s (c defaultClass) [ ( "cursor", "default" ) ]
    , s (c pointerClass) [ ( "cursor", "pointer" ) ]
    , s (c allScrollClass) [ ( "cursor", "all-scroll" ) ]
    , s (c zoomInClass) [ ( "cursor", "zoom-in" ) ]
    ]


defaultClass : String
defaultClass =
    "crsrDflt"


pointerClass : String
pointerClass =
    "crsrPntr"


allScrollClass : String
allScrollClass =
    "crsrAlls"


zoomInClass : String
zoomInClass =
    "crsrZmin"
