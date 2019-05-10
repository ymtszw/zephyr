module View.Organisms.Column.Items.ItemForView.EmbeddedMatter exposing
    ( EmbeddedMatter, Thumbnail, new
    , color, pretext, author, title, url, kts, thumbnail, attachedFiles, origin
    )

{-| EmbeddedMatter in ColumnItem, and its builder functions.

@docs EmbeddedMatter, Thumbnail, new
@docs color, pretext, author, title, url, kts, thumbnail, attachedFiles, origin

-}

import Color exposing (Color)
import View.Organisms.Column.Items.ItemForView.Contents exposing (..)
import View.Organisms.Column.Items.ItemForView.NamedEntity exposing (NamedEntity)


type alias EmbeddedMatter =
    { color : Maybe Color
    , pretext : Maybe Text -- Mainly for Slack, leading text above guttered emebed block
    , author : Maybe NamedEntity
    , title : Maybe Text
    , url : Maybe String
    , body : Text
    , kts : KTS
    , thumbnail : Maybe Thumbnail
    , attachedFiles : List AttachedFile
    , origin : Maybe NamedEntity -- E.g. source service name
    }


type alias Thumbnail =
    -- == MediaRecord {}
    { src : String
    , link : String
    , description : String
    , dimension : Maybe { width : Int, height : Int }
    }


new : Text -> EmbeddedMatter
new body =
    { color = Nothing
    , author = Nothing
    , pretext = Nothing
    , title = Nothing
    , url = Nothing
    , body = body
    , kts = []
    , thumbnail = Nothing
    , attachedFiles = []
    , origin = Nothing
    }


color : Color -> EmbeddedMatter -> EmbeddedMatter
color val old =
    { old | color = Just val }


pretext : Text -> EmbeddedMatter -> EmbeddedMatter
pretext val old =
    { old | pretext = Just val }


author : NamedEntity -> EmbeddedMatter -> EmbeddedMatter
author val old =
    { old | author = Just val }


title : Text -> EmbeddedMatter -> EmbeddedMatter
title val old =
    { old | title = Just val }


url : String -> EmbeddedMatter -> EmbeddedMatter
url val old =
    { old | url = Just val }


kts : KTS -> EmbeddedMatter -> EmbeddedMatter
kts val old =
    { old | kts = val }


thumbnail : Thumbnail -> EmbeddedMatter -> EmbeddedMatter
thumbnail val old =
    { old | thumbnail = Just val }


attachedFiles : List AttachedFile -> EmbeddedMatter -> EmbeddedMatter
attachedFiles val old =
    { old | attachedFiles = val }


origin : NamedEntity -> EmbeddedMatter -> EmbeddedMatter
origin val old =
    { old | origin = Just val }
