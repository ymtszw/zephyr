module View.Organisms.Column.Items.ItemForView.Contents exposing
    ( Text(..), KTS, AttachedFile(..), VisualMedia(..), MediaRecord, FileUrl(..)
    , unwrapVisualMedia, imageMedia, videoMedia
    , attachedImage, attachedVideo, attachedYoutube, attachedTwitchChannel, attachedTwitchClip, attachedOther
    , attachedFileLink, attachedFileDescription, attachedFilePreview, attachedFileDimension, attachedFilePoster
    )

{-| Contents of ColumnItem, and some builder functions.

@docs Text, KTS, AttachedFile, VisualMedia, MediaRecord, FileUrl
@docs unwrapVisualMedia, imageMedia, videoMedia
@docs attachedImage, attachedVideo, attachedYoutube, attachedTwitchChannel, attachedTwitchClip, attachedOther
@docs attachedFileLink, attachedFileDescription, attachedFilePreview, attachedFileDimension, attachedFilePoster

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


unwrapVisualMedia : AttachedFile -> Maybe VisualMedia
unwrapVisualMedia af =
    case af of
        VisualFile visualMedia ->
            Just visualMedia

        OtherFile _ ->
            Nothing


type VisualMedia
    = Image (MediaRecord {})
    | Video (MediaRecord { poster : Maybe String })
    | Youtube ExternalServiceResourceRecord
    | TwitchChannel ExternalServiceResourceRecord
    | TwitchClip ExternalServiceResourceRecord


type alias MediaRecord a =
    { a
        | src : String
        , link : String
        , description : String
        , dimension : Maybe { width : Int, height : Int }
    }


type alias ExternalServiceResourceRecord =
    { id : String
    , poster : Maybe String
    , dimension : Maybe { width : Int, height : Int }
    }


imageMedia : String -> String -> String -> Maybe { width : Int, height : Int } -> VisualMedia
imageMedia src link description dimension =
    Image
        { src = src
        , link = link
        , description = description
        , dimension = dimension
        }


videoMedia : String -> String -> String -> Maybe { width : Int, height : Int } -> Maybe String -> VisualMedia
videoMedia src link description dimension poster =
    Video
        { src = src
        , link = link
        , description = description
        , dimension = dimension
        , poster = poster
        }


youtubeMedia : String -> Maybe String -> Maybe { width : Int, height : Int } -> VisualMedia
youtubeMedia id poster dimension =
    Youtube (ExternalServiceResourceRecord id poster dimension)


twitchChannelMedia : String -> Maybe String -> Maybe { width : Int, height : Int } -> VisualMedia
twitchChannelMedia id poster dimension =
    TwitchChannel (ExternalServiceResourceRecord id poster dimension)


twitchClipMedia : String -> Maybe String -> Maybe { width : Int, height : Int } -> VisualMedia
twitchClipMedia slug poster dimension =
    TwitchClip (ExternalServiceResourceRecord slug poster dimension)


attachedImage : String -> AttachedFile
attachedImage src =
    VisualFile (imageMedia src src "Attached image" Nothing)


attachedVideo : String -> AttachedFile
attachedVideo src =
    VisualFile (videoMedia src src "Attached video" Nothing Nothing)


attachedYoutube : String -> AttachedFile
attachedYoutube id =
    VisualFile (youtubeMedia id Nothing Nothing)


attachedTwitchChannel : String -> AttachedFile
attachedTwitchChannel id =
    VisualFile (twitchChannelMedia id Nothing Nothing)


attachedTwitchClip : String -> AttachedFile
attachedTwitchClip slug =
    VisualFile (twitchClipMedia slug Nothing Nothing)


attachedOther : FileUrl -> AttachedFile
attachedOther fileUrl =
    OtherFile { fileUrl = fileUrl, description = "Attached file", preview = Nothing }


attachedFileLink : String -> AttachedFile -> AttachedFile
attachedFileLink link f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image { record | link = link })

        VisualFile (Video record) ->
            VisualFile (Video { record | link = link })

        VisualFile (Youtube record) ->
            VisualFile (Youtube record)

        VisualFile (TwitchChannel record) ->
            VisualFile (TwitchChannel record)

        VisualFile (TwitchClip record) ->
            VisualFile (TwitchClip record)

        OtherFile record ->
            OtherFile record


attachedFileDescription : String -> AttachedFile -> AttachedFile
attachedFileDescription description f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image { record | description = description })

        VisualFile (Video record) ->
            VisualFile (Video { record | description = description })

        VisualFile (Youtube record) ->
            VisualFile (Youtube record)

        VisualFile (TwitchChannel record) ->
            VisualFile (TwitchChannel record)

        VisualFile (TwitchClip record) ->
            VisualFile (TwitchClip record)

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

        VisualFile (Youtube record) ->
            VisualFile (Youtube { record | dimension = Just dimension })

        VisualFile (TwitchChannel record) ->
            VisualFile (TwitchChannel { record | dimension = Just dimension })

        VisualFile (TwitchClip record) ->
            VisualFile (TwitchClip { record | dimension = Just dimension })

        OtherFile record ->
            OtherFile record


type FileUrl
    = DownloadUrl String
    | ExternalLink String


attachedFilePoster : String -> AttachedFile -> AttachedFile
attachedFilePoster poster f =
    case f of
        VisualFile (Image record) ->
            VisualFile (Image record)

        VisualFile (Video record) ->
            VisualFile (Video { record | poster = Just poster })

        VisualFile (Youtube record) ->
            VisualFile (Youtube { record | poster = Just poster })

        VisualFile (TwitchChannel record) ->
            VisualFile (TwitchChannel { record | poster = Just poster })

        VisualFile (TwitchClip record) ->
            VisualFile (TwitchClip { record | poster = Just poster })

        OtherFile record ->
            OtherFile record
