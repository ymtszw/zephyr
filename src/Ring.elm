module Ring exposing (Ring(..), cons, consIf, fromList, init, toList)

import Array exposing (Array)


type Ring a
    = Ring { buffer : List a, limit : Int }


init : Int -> Ring a
init limit =
    fromList limit []


sanitizeLimit : Int -> Int
sanitizeLimit limit =
    if limit < minLimit then
        minLimit

    else
        limit


minLimit : Int
minLimit =
    1


fromList : Int -> List a -> Ring a
fromList limit list =
    Ring
        { buffer = List.take (sanitizeLimit limit) list
        , limit = sanitizeLimit limit
        }


toList : Ring a -> List a
toList (Ring r) =
    r.buffer


cons : a -> Ring a -> Ring a
cons item (Ring r) =
    Ring { r | buffer = List.take r.limit (item :: r.buffer) }


consIf : (a -> a -> Bool) -> a -> Ring a -> Ring a
consIf checker item (Ring r) =
    case r.buffer of
        [] ->
            cons item (Ring r)

        x :: xs ->
            if checker x item then
                cons item (Ring r)

            else
                Ring r
