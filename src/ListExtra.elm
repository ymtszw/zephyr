module ListExtra exposing (groupWhile)


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile checkFromPrev list =
    -- This is actually equivalent to List.Extra.groupWhileTransitively
    let
        reducer item acc =
            case acc of
                [] ->
                    [ [ item ] ]

                group :: groups ->
                    case group of
                        [] ->
                            -- This should not actually happen
                            [ item ] :: groups

                        x :: xs ->
                            if checkFromPrev x item then
                                (item :: group) :: groups

                            else
                                [ item ] :: group :: groups
    in
    list |> List.foldl reducer [] |> reverseMap List.reverse


reverseMap : (a -> b) -> List a -> List b
reverseMap mapper =
    reverseMapImpl mapper []


reverseMapImpl : (a -> b) -> List b -> List a -> List b
reverseMapImpl mapper acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            reverseMapImpl mapper (mapper x :: acc) xs
