module Data.ColumnItem.EmbeddedBlock exposing
    ( EmbeddedBlock, new
    , color, author, title, url, kts, thumbnail, attachedFiles, origin
    )

{-| EmbeddedBlock in ColumnItem, and its builder functions.

@docs EmbeddedBlock, new
@docs color, author, title, url, kts, thumbnail, attachedFiles, origin

-}

import Color exposing (Color)
import Data.ColumnItem.Contents exposing (..)
import Data.ColumnItem.NamedEntity exposing (NamedEntity)


type alias EmbeddedBlock =
    { color : Maybe Color
    , author : Maybe NamedEntity
    , title : Maybe String
    , url : Maybe String
    , body : Text
    , kts : KTS
    , thumbnail : Maybe VisualMedia
    , attachedFiles : List AttachedFile
    , origin : Maybe NamedEntity -- E.g. source service name
    }


new : Text -> EmbeddedBlock
new body =
    { color = Nothing
    , author = Nothing
    , title = Nothing
    , url = Nothing
    , body = body
    , kts = []
    , thumbnail = Nothing
    , attachedFiles = []
    , origin = Nothing
    }


color : Color -> EmbeddedBlock -> EmbeddedBlock
color val old =
    { old | color = Just val }


author : NamedEntity -> EmbeddedBlock -> EmbeddedBlock
author val old =
    { old | author = Just val }


title : String -> EmbeddedBlock -> EmbeddedBlock
title val old =
    { old | title = Just val }


url : String -> EmbeddedBlock -> EmbeddedBlock
url val old =
    { old | url = Just val }


kts : KTS -> EmbeddedBlock -> EmbeddedBlock
kts val old =
    { old | kts = val }


thumbnail : VisualMedia -> EmbeddedBlock -> EmbeddedBlock
thumbnail val old =
    { old | thumbnail = Just val }


attachedFiles : List AttachedFile -> EmbeddedBlock -> EmbeddedBlock
attachedFiles val old =
    { old | attachedFiles = val }


origin : NamedEntity -> EmbeddedBlock -> EmbeddedBlock
origin val old =
    { old | origin = Just val }
