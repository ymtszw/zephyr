module View.Style exposing (Style, c, derive, inject, kf, pure, px, s, scale12, toString)

import Dict exposing (Dict)


{-| An entry in stylesheet.
-}
type Style
    = RawStyle Raw
    | KeyFrames KF


type KF
    = KF String (List Raw)


type Raw
    = Raw String (Dict String String)


{-| Produces a CSS entry for a selector.

    toString (s "p" [ ( "color", "red" ) ])
    --> "p{color:red;}"

-}
s : String -> List ( String, String ) -> Style
s selector props =
    RawStyle (Raw selector (Dict.fromList props))


{-| Creates an empty CSS entry for a selector.

You will use this with `inject`.

-}
pure : String -> Style
pure selector =
    RawStyle (Raw selector Dict.empty)


{-| Produces a CSS entry for a class.

    toString (c "foo" [ ( "color", "red" ) ])
    --> ".foo{color:red;}"

-}
c : String -> List ( String, String ) -> Style
c className props =
    RawStyle (Raw ("." ++ className) (Dict.fromList props))


{-| Produces a `@keyframes` entry.

    toString <| kf "rotating"
        [ ( "from", [ ("transform", "rotate(0turn)" ) ] )
        , ( "to", [ ("transform", "rotate(1turn)" ) ] )
        ]
    --> "@keyframes rotating{from{transform:rotate(0turn);}to{transform:rotate(1turn);}}"

-}
kf : String -> List ( String, List ( String, String ) ) -> Style
kf animationName frameBlocks =
    KeyFrames <|
        KF animationName <|
            List.map (\( frameSelector, props ) -> Raw frameSelector (Dict.fromList props)) frameBlocks


{-| Derives a new Style from another.
-}
derive : String -> Style -> Style
derive selector base =
    inject base (pure selector)


{-| Injects properties in a Style (first one) into another (second) one,
effectively combining styles into one.

The second one is considered "base", and the first one is a "mixin".
If properties collide, ones in the first Style ("new" one) take effect.

You can only combine RawStyles currently. KeyFrames are kept intact.

-}
inject : Style -> Style -> Style
inject new base =
    case base of
        RawStyle (Raw selector baseProps) ->
            case new of
                KeyFrames _ ->
                    base

                RawStyle (Raw _ newProps) ->
                    RawStyle (Raw selector (Dict.union newProps baseProps))

        KeyFrames _ ->
            base


toString : Style -> String
toString style =
    case style of
        RawStyle raw ->
            rawToString raw

        KeyFrames (KF animationName frameBlocks) ->
            String.join ""
                [ "@keyframes "
                , animationName
                , "{"
                , String.join "" (List.map rawToString frameBlocks)
                , "}"
                ]


rawToString : Raw -> String
rawToString (Raw selector props) =
    String.join ""
        [ selector
        , "{"
        , Dict.foldl (\name value acc -> acc ++ name ++ ":" ++ value ++ ";") "" props
        , "}"
        ]



-- HELPERS


px : Int -> String
px i =
    String.fromInt i ++ "px"


scale12 : Int -> Int
scale12 =
    scaleByQuarter 12


scaleByQuarter : Int -> Int -> Int
scaleByQuarter base factor =
    if factor == 0 then
        base

    else
        floor (toFloat base * 1.25 ^ toFloat factor)
