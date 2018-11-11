module Data.Storable exposing (Storable, append, encode, finalize)

{-| Objects that can be stored to IndexedDB.
-}

import Dict exposing (Dict)
import Json.Encode as E


type Storable
    = Storable (Dict String E.Value)


{-| Encode to JSON object that can be stored to IndexedDB.
-}
encode : String -> List ( String, E.Value ) -> Storable
encode id props =
    Storable (Dict.fromList (( "id", E.string id ) :: props))


{-| Append additional fields to Sortable.

If any fields collide, new ones overwrite old ones.

-}
append : List ( String, E.Value ) -> Storable -> Storable
append newProps (Storable props) =
    Storable (Dict.union (Dict.fromList newProps) props)


finalize : Storable -> E.Value
finalize (Storable v) =
    E.dict identity identity v
