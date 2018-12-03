module SelectArray exposing
    ( SelectArray, singleton, fromLists
    , selected, selectedIndex, size, selectAt, updateSelected, indexedMap
    )

{-| A Zipper backed by Array.

Why Array? Because:

  - Indexed access and boundary (length) check are cheap and fast
  - In my use case, I want to select by global index, rather than relatively from previous selection
  - Overall performance is not measured. Could be worse. LUL

@docs SelectArray, singleton, fromLists
@docs selected, selectedIndex, size, selectAt, updateSelected, indexedMap

-}

import Array exposing (..)


type SelectArray a
    = SelectArray
        { front : Array a
        , selected : a
        , rear : Array a
        }


singleton : a -> SelectArray a
singleton a =
    SelectArray { front = empty, selected = a, rear = empty }


fromLists : List a -> a -> List a -> SelectArray a
fromLists front selected_ rear =
    SelectArray { front = fromList front, selected = selected_, rear = fromList rear }


selected : SelectArray a -> a
selected (SelectArray sa) =
    sa.selected


selectedIndex : SelectArray a -> Int
selectedIndex (SelectArray sa) =
    length sa.front


size : SelectArray a -> Int
size (SelectArray sa) =
    length sa.front + 1 + length sa.rear


{-| For the sake of simplicity, negative indexes are not allowed.
-}
selectAt : Int -> SelectArray a -> SelectArray a
selectAt index (SelectArray sa) =
    let
        frontLen =
            length sa.front

        rearLen =
            length sa.rear

        totalLen =
            frontLen + 1 + rearLen
    in
    if index < 0 || index == frontLen || totalLen <= index then
        SelectArray sa

    else if 0 <= index && index < frontLen then
        case get index sa.front of
            Just a ->
                SelectArray
                    { front = slice 0 index sa.front
                    , selected = a
                    , rear = append (push sa.selected (slice (index + 1) frontLen sa.front)) sa.rear
                    }

            Nothing ->
                -- Should not happen
                SelectArray sa

    else
        let
            rearIndex =
                index - frontLen - 1
        in
        case get rearIndex sa.rear of
            Just a ->
                SelectArray
                    { front = append (push sa.selected sa.front) (slice 0 rearIndex sa.rear)
                    , selected = a
                    , rear = slice (rearIndex + 1) rearLen sa.rear
                    }

            Nothing ->
                SelectArray sa


updateSelected : (a -> a) -> SelectArray a -> SelectArray a
updateSelected updater (SelectArray sa) =
    SelectArray { sa | selected = updater sa.selected }


indexedMap : ({ selected : Bool, index : Int, e : a } -> b) -> SelectArray a -> List b
indexedMap mapper (SelectArray sa) =
    let
        frontList =
            List.indexedMap (\i e -> mapper { selected = False, index = i, e = e }) (Array.toList sa.front)

        frontLen =
            Array.length sa.front

        selected_ =
            mapper { selected = True, index = frontLen, e = sa.selected }

        rearList =
            List.indexedMap (\i e -> mapper { selected = False, index = frontLen + 1 + i, e = e }) (Array.toList sa.rear)
    in
    frontList ++ selected_ :: rearList
