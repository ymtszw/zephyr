module Data.ColumnItem.Contents exposing
    ( Text(..), KTS, AttachedFile(..), VisualMedia(..), MediaRecord, FileUrl(..)
    , imageMedia, videoMedia
    , attachedImage, attachedVideo, attachedOther, attachedFileDescription, attachedFilePreview, attachedFileDimension
    )

{-| Contents of ColumnItem, and some builder functions.

@docs Text, KTS, AttachedFile, VisualMedia, MediaRecord, FileUrl
@docs imageMedia, videoMedia
@docs attachedImage, attachedVideo, attachedOther, attachedFileDescription, attachedFilePreview, attachedFileDimension

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
        , description : String
        , preview : Maybe String -- If present, it is rendered in code block. Non-text previews are not supported
        }


type VisualMedia
    = Image MediaRecord
    | Video MediaRecord


type alias MediaRecord =
    -- TODO Add link field for canonical permalink?
    { src : String
    , description : String
    , dimension : Maybe { width : Int, height : Int }
    }


imageMedia : String -> String -> Maybe { width : Int, height : Int } -> VisualMedia
imageMedia src description dimension =
    Image (MediaRecord src description dimension)


videoMedia : String -> String -> Maybe { width : Int, height : Int } -> VisualMedia
videoMedia src description dimension =
    Video (MediaRecord src description dimension)


attachedImage : String -> AttachedFile
attachedImage src =
    VisualFile (imageMedia src "Attached image" Nothing)


attachedVideo : String -> AttachedFile
attachedVideo src =
    VisualFile (videoMedia src "Attached video" Nothing)


attachedOther : FileUrl -> AttachedFile
attachedOther fileUrl =
    OtherFile { fileUrl = fileUrl, description = "Attached file", preview = Nothing }


attachedFileDescription : String -> AttachedFile -> AttachedFile
attachedFileDescription description f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image { record | description = description })

        VisualFile (Video record) ->
            VisualFile (Video { record | description = description })

        OtherFile record ->
            OtherFile { record | description = description }


attachedFilePreview : String -> AttachedFile -> AttachedFile
attachedFilePreview preview f =
    case f of
        VisualFile media ->
            VisualFile media

        OtherFile record ->
            OtherFile { record | preview = Just preview }


attachedFileDimension : { width : Int, height : Int } -> AttachedFile -> AttachedFile
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
