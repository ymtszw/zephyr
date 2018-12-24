module View.Style exposing (Style, c, kf, px, s, scale12, toString)


type Style
    = KeyFrames KF
    | RawStyle Raw


type KF
    = KF String (List Raw)


type Raw
    = Raw String (List ( String, String ))


{-| Produces a CSS entry for a selector.

    s "p" [ ( "color", "red" ) ]
    --> "p{color:red;}"

-}
s : String -> List ( String, String ) -> Style
s selector props =
    RawStyle (Raw selector props)


{-| Produces a CSS entry for a class.

    c "foo" [ ( "color", "red" ) ] --> ".foo{color:red;}"

-}
c : String -> List ( String, String ) -> Style
c className props =
    RawStyle (Raw ("." ++ className) props)


{-| Produces a `@keyframes` entry.

    kf "rotating"
        [ ( "from", [ ("transform", "rotate(0turn)" ) ] )
        , ( "to", [ ("transform", "rotate(1turn)" ) ] )
        ]
    --> "@keyframes rotating{from{transform:rotate(0turn);}to{transform:rotate(1turn);}}"

-}
kf : String -> List ( String, List ( String, String ) ) -> Style
kf animationName frameBlocks =
    KeyFrames <|
        KF animationName <|
            List.map (\( frameSelector, props ) -> Raw frameSelector props) frameBlocks


toString : Style -> String
toString style =
    case style of
        KeyFrames (KF animationName frameBlocks) ->
            String.join ""
                [ "@keyframes "
                , animationName
                , "{"
                , String.join "" (List.map rawToString frameBlocks)
                , "}"
                ]

        RawStyle raw ->
            rawToString raw


rawToString : Raw -> String
rawToString (Raw selector props) =
    String.join ""
        [ selector
        , "{"
        , String.join "" (List.map (\( name, value ) -> name ++ ":" ++ value ++ ";") props)
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
