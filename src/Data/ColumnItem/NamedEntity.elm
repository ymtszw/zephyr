module Data.ColumnItem.NamedEntity exposing
    ( NamedEntity, Avatar(..), desiredIconSize, new
    , secondaryName, avatar, url, imageOrAbbr
    )

{-| NamedEntity (mainly author information) in ColumnItem, and its builder functions.

@docs NamedEntity, Avatar, desiredIconSize, new
@docs secondaryName, avatar, url, imageOrAbbr

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


desiredIconSize : Int
desiredIconSize =
    40


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


imageOrAbbr : Maybe String -> String -> Bool -> Avatar
imageOrAbbr src name isBot =
    ImageOrAbbr { src = src, name = name, isBot = isBot }
