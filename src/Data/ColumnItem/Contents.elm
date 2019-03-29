module Data.ColumnItem.Contents exposing (AttachedFile(..), FileUrl(..), KTS, MediaRecord, Text(..), VisualMedia(..))


type Text
    = Plain String
    | Markdown String -- Parsed then rendered


type alias KTS =
    -- Any key-text pairs; if messages have something non-generic components,
    -- you may dump them here to tentatively show them
    List ( String, Text )


type AttachedFile
    = VisualFile VisualMedia
    | OtherFile
        { fileUrl : FileUrl
        , description : Maybe String
        , preview : Maybe String -- If present, it is rendered in code block. Non-text previews are not supported
        }


type VisualMedia
    = Image MediaRecord
    | Video MediaRecord


type alias MediaRecord =
    { src : String
    , description : Maybe String
    , dimension : Maybe ( Int, Int )
    }


type FileUrl
    = DownloadUrl String
    | ExternalLink String
