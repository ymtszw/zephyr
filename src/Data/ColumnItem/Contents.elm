module Data.ColumnItem.Contents exposing
    ( Text(..), KTS, AttachedFile(..), VisualMedia(..), MediaRecord, FileUrl(..)
    , attachedImage, attachedVideo, attachedFileDescription, attachedFilePreview, attachedFileDimension
    )

{-| Contents of ColumnItem, and some builder functions.

@docs Text, KTS, AttachedFile, VisualMedia, MediaRecord, FileUrl
@docs attachedImage, attachedVideo, attachedFileDescription, attachedFilePreview, attachedFileDimension

-}


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
    , description : String
    , dimension : Maybe ( Int, Int )
    }


attachedImage : String -> AttachedFile
attachedImage src =
    VisualFile (Image (MediaRecord src "attached image" Nothing))


attachedVideo : String -> AttachedFile
attachedVideo src =
    VisualFile (Video (MediaRecord src "attached video" Nothing))


attachedFileDescription : String -> AttachedFile -> AttachedFile
attachedFileDescription description f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image { record | description = description })

        VisualFile (Video record) ->
            VisualFile (Video { record | description = description })

        OtherFile record ->
            OtherFile { record | description = Just description }


attachedFilePreview : String -> AttachedFile -> AttachedFile
attachedFilePreview preview f =
    case f of
        VisualFile media ->
            VisualFile media

        OtherFile record ->
            OtherFile { record | preview = Just preview }


attachedFileDimension : ( Int, Int ) -> AttachedFile -> AttachedFile
attachedFileDimension dimension f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image { record | dimension = Just dimension })

        VisualFile (Video record) ->
            VisualFile (Video { record | dimension = Just dimension })

        OtherFile record ->
            OtherFile record


type FileUrl
    = DownloadUrl String
    | ExternalLink String
