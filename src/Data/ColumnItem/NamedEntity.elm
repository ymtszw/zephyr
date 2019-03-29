module Data.ColumnItem.NamedEntity exposing
    ( NamedEntity, Avatar(..), new
    , secondaryName, avatar, url
    )

{-| NamedEntity (mainly author information) in ColumnItem, and its builder functions.

@docs NamedEntity, Avatar, new
@docs secondaryName, avatar, url

-}


type alias NamedEntity =
    { primaryName : String
    , secondaryName : Maybe String
    , avatar : Maybe Avatar
    , url : Maybe String
    }


type Avatar
    = OcticonInfo
    | OcticonNote
    | ImageOrAbbr { src : Maybe String, name : String, isBot : Bool }


new : String -> NamedEntity
new primaryName =
    { primaryName = primaryName
    , secondaryName = Nothing
    , avatar = Nothing
    , url = Nothing
    }


secondaryName : String -> NamedEntity -> NamedEntity
secondaryName val old =
    { old | secondaryName = Just val }


avatar : Avatar -> NamedEntity -> NamedEntity
avatar val old =
    { old | avatar = Just val }


url : String -> NamedEntity -> NamedEntity
url val old =
    { old | url = Just val }
