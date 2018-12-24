module View.Style exposing (Style, kf, s, toString)


type Style
    = KeyFrames KF
    | RawStyle Raw


type KF
    = KF String (List Raw)


type Raw
    = Raw String (List ( String, String ))


s : String -> List ( String, String ) -> Style
s selector props =
    RawStyle (Raw selector props)


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
