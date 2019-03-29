module Data.ColumnItem.NamedEntity exposing
    ( NamedEntity, Avatar(..), new
    , secondary, avatar, url
    )

{-| NamedEntity (mainly author information) in ColumnItem, and its builder functions.

@docs NamedEntity, Avatar, new
@docs secondary, avatar, url

-}


type alias NamedEntity =
    { primary : String
    , secondary : Maybe String
    , avatar : Maybe Avatar
    , url : Maybe String
    }


type Avatar
    = OcticonInfo
    | OcticonNote
    | ImageOrAbbr { src : Maybe String, name : String, isBot : Bool }


new : String -> NamedEntity
new primary =
    { primary = primary
    , secondary = Nothing
    , avatar = Nothing
    , url = Nothing
    }


secondary : String -> NamedEntity -> NamedEntity
secondary val old =
    { old | secondary = Just val }


avatar : Avatar -> NamedEntity -> NamedEntity
avatar val old =
    { old | avatar = Just val }


url : String -> NamedEntity -> NamedEntity
url val old =
    { old | url = Just val }
