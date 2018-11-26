module Logger.Entry exposing (Entry)


type alias Entry =
    { ctor : String
    , payload : List String
    }
