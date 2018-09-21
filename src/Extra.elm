module Extra exposing (ite)

{-| Basics.Extra. Provides frequently used idiomatic helper.
-}


{-| Oneline if-then-else.

Avoiding elm-format expansion.

-}
ite : Bool -> a -> a -> a
ite condition then_ else_ =
    if condition then
        then_

    else
        else_
