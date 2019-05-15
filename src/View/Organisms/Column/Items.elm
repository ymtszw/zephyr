module View.Organisms.Column.Items exposing (render, styles)

import Color
import Data.Column as Column
import Html exposing (Html, div, img, p, pre, span, video)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Octicons
import StringExtra
import TextParser
import Time
import TimeExtra
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.TextBlock exposing (breakWords, clip, nowrap)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Molecules.MarkdownBlocks as MarkdownBlocks
import View.Organisms.Column.Items.ItemForView exposing (ItemForView)
import View.Organisms.Column.Items.ItemForView.Contents exposing (..)
import View.Organisms.Column.Items.ItemForView.EmbeddedMatter exposing (EmbeddedMatter)
import View.Organisms.Column.Items.ItemForView.NamedEntity exposing (Avatar(..))
import View.Style exposing (..)


type alias Effects msg =
    { onLoadMoreClick : msg
    , onItemSourceButtonClick : Column.Id -> Int -> msg
    , onItemRefreshButtonClick : Column.Id -> Int -> msg
    , onItemMediaClick : Column.Id -> Int -> Int -> msg
    }


type alias Props =
    { timezone : Time.Zone
    , columnId : Column.Id
    , hasMore : Bool
    , -- Expects it to be sorted from latest to oldest (globally), while reversed within each group.
      itemGroups : List ( ItemForView, List ItemForView )
    }


render : Effects msg -> Props -> Html msg
render eff props =
    case props.itemGroups of
        [] ->
            div [ flexColumn, flexCenter, padding15, colorNote ] [ t "Waiting for messages..." ]

        itemGroups ->
            let
                contents =
                    List.map (itemGroupKey eff props) itemGroups
            in
            Html.Keyed.node "div"
                [ flexBasisAuto
                , flexShrink
                , flexColumn
                ]
                (contents ++ [ loadMoreOrBottomMarkerKey eff.onLoadMoreClick props.hasMore ])


loadMoreOrBottomMarkerKey : msg -> Bool -> ( String, Html msg )
loadMoreOrBottomMarkerKey onLoadMoreClick hasMore =
    -- This clickable area is rarely visible due to auto adjusting.
    -- But sometimes appears around window resizing, rapid scrolling, or reaching bottom
    let
        ( shape, additionalAttrs ) =
            if hasMore then
                ( Octicons.commentDiscussion
                , [ title "Scroll, or click here for more...", onClick onLoadMoreClick, Cursor.pointer ]
                )

            else
                ( Octicons.thumbsup, [ title "All read!" ] )
    in
    Tuple.pair "loadMoreOrBottomMarker" <|
        div ([ flexColumn, flexCenter, padding15 ] ++ additionalAttrs)
            [ Image.octicon
                { size = xxProminentSize
                , shape = shape
                }
            ]



-- ITEM


itemGroupKey : Effects msg -> Props -> ( ItemForView, List ItemForView ) -> ( String, Html msg )
itemGroupKey eff props ( oldestItem, subsequentItems ) =
    Tuple.pair ("itemGroup_" ++ oldestItem.id) <|
        div
            [ class itemGroupClass
            , flexRow
            , flexBasisAuto
            , Border.bot1
            , Border.solid
            , Border.colorBd
            ]
            [ itemAuthorAvatar40 oldestItem
            , itemGroupContents eff props oldestItem subsequentItems
            ]


itemAuthorAvatar40 : ItemForView -> Html msg
itemAuthorAvatar40 item =
    let
        octiconAvatar40 shape =
            Icon.octiconBlock
                [ alignStart
                , Icon.rounded40
                , Border.colorNote
                ]
                { size = xProminentSize
                , shape = shape
                }

        wrapInLink child =
            case item.author.url of
                Just url ->
                    ntLink [] { url = url, children = [ child ] }

                Nothing ->
                    child
    in
    wrapInLink <|
        case item.author.avatar of
            Just OcticonInfo ->
                octiconAvatar40 Octicons.info

            Just OcticonNote ->
                octiconAvatar40 Octicons.note

            Just (ImageOrAbbr opts) ->
                withBadge [ alignStart, badgeOutset ]
                    { topRight = Nothing
                    , bottomRight =
                        if opts.isBot then
                            Just Icon.botBadge14

                        else
                            Nothing
                    , content = Icon.imgOrAbbr [ serif, xProminent, Icon.rounded40 ] opts.name opts.src
                    }

            Nothing ->
                Icon.imgOrAbbr [ serif, xProminent, alignStart, Icon.rounded40 ] item.author.primaryName Nothing


itemGroupContents : Effects msg -> Props -> ItemForView -> List ItemForView -> Html msg
itemGroupContents eff props oldestItem subsequentItems =
    let
        bodyBlocks =
            List.map (itemBlockKey eff props) (oldestItem :: subsequentItems)
    in
    Html.Keyed.node "div"
        [ class itemGroupContentsClass
        , clip
        , flexColumn
        , flexBasisAuto
        , flexGrow
        , flexShrink
        , spacingColumn2
        ]
        (itemGroupHeaderKey oldestItem :: bodyBlocks)


itemGroupHeaderKey : ItemForView -> ( String, Html msg )
itemGroupHeaderKey item =
    Tuple.pair "itemGroupHeader" <|
        div [ flexRow, flexCenter, spacingRow2 ]
            [ div [ flexShrink, flexBasisAuto, breakWords, bold, prominent ] [ t item.author.primaryName ]
            , case item.author.secondaryName of
                Just secondaryName ->
                    div [ colorNote, flexShrink, flexBasisAuto, breakWords ] [ t secondaryName ]

                Nothing ->
                    none
            ]


itemBlockKey : Effects msg -> Props -> ItemForView -> ( String, Html msg )
itemBlockKey eff props item =
    Tuple.pair item.id <|
        withBadge
            [ class itemBlockClass
            , flexGrow
            , Background.hovSub
            ]
            { topRight = Just (hoverMenu eff props item)
            , bottomRight = Nothing
            , content =
                let
                    mediaIndex0 =
                        0

                    onMediaClick =
                        eff.onItemMediaClick props.columnId item.scrollIndex

                    ( embeddedMatterBlocks, mediaIndex1 ) =
                        List.foldr (embeddedMatterBlockAndPretext onMediaClick) ( [], mediaIndex0 ) item.embeddedMatters

                    ( attachedFileBlocks, _ ) =
                        List.foldr (attachedFileBlock onMediaClick) ( [], mediaIndex1 ) item.attachedFiles

                    children =
                        textBlocks item.body
                            ++ embeddedMatterBlocks
                            ++ List.map ktBlock item.kts
                            -- TODO reactions
                            ++ attachedFileBlocks
                in
                div
                    [ flexColumn
                    , flexBasisAuto
                    , flexShrink
                    , flexGrow
                    , spacingColumn5
                    ]
                    children
            }


hoverMenu : Effects msg -> Props -> ItemForView -> Html msg
hoverMenu eff props item =
    let
        menuOcticonButton onPress shape =
            Icon.octiconButton [ flexItem, padding2, Image.hovText, Background.transparent, Border.round5 ]
                { onPress = onPress props.columnId item.scrollIndex
                , size = regularSize
                , shape = shape
                }
    in
    div [ class flexHoverMenuClass, colorNote, minuscule, padding2, spacingRow2, Background.colorMain ] <|
        case item.timestamp of
            Just posixTime ->
                [ div [ nowrap, padding5 ] [ t (TimeExtra.local props.timezone posixTime) ]
                , menuOcticonButton eff.onItemRefreshButtonClick Octicons.sync
                , menuOcticonButton eff.onItemSourceButtonClick Octicons.code
                ]

            Nothing ->
                [ menuOcticonButton eff.onItemRefreshButtonClick Octicons.sync
                , menuOcticonButton eff.onItemSourceButtonClick Octicons.code
                ]


ktBlock : ( String, Text ) -> Html msg
ktBlock ( key, text ) =
    div []
        [ div [ bold ] [ t key ]
        , div [ padding2 ] (textBlocks text)
        ]


textBlocks : Text -> List (Html msg)
textBlocks text =
    case text of
        Plain "" ->
            []

        Plain string ->
            [ p [ breakWords ] [ t string ] ]

        Markdown string ->
            markdownBlocks string


markdownBlocks : String -> List (Html msg)
markdownBlocks raw =
    if String.isEmpty raw then
        []

    else
        -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
        MarkdownBlocks.render TextParser.defaultOptions raw


embeddedMatterBlockAndPretext : (Int -> msg) -> EmbeddedMatter -> ( List (Html msg), Int ) -> ( List (Html msg), Int )
embeddedMatterBlockAndPretext onMediaClick matter ( accBlocks, mediaIndex ) =
    let
        wrapInLink urlMaybe children =
            case urlMaybe of
                Just url ->
                    [ ntLink [] { url = url, children = children } ]

                Nothing ->
                    children

        gutterColor =
            Maybe.withDefault Color.gray matter.color

        ( textContentsAndThumbnailBlock, mediaIndexAfterThumb ) =
            case matter.thumbnail of
                Just thumbnail ->
                    ( [ div [ class thumbnailParentClass, flexRow, flexBasisAuto, spacingRow2 ]
                            [ div [ flexGrow ] <| authorBlock ++ titleBlock ++ textBlocks matter.body ++ List.map ktBlock matter.kts
                            , img
                                [ flexItem
                                , flexShrink
                                , flexBasisAuto
                                , alignStart
                                , src thumbnail.src
                                , alt thumbnail.description
                                , Cursor.zoomIn
                                , onClick (onMediaClick mediaIndex)
                                ]
                                []
                            ]
                      ]
                    , mediaIndex + 1
                    )

                Nothing ->
                    ( authorBlock ++ titleBlock ++ textBlocks matter.body ++ List.map ktBlock matter.kts
                    , mediaIndex
                    )

        authorBlock =
            case matter.author of
                Just namedEntity ->
                    [ div [] <|
                        wrapInLink namedEntity.url <|
                            let
                                avatar =
                                    case namedEntity.avatar of
                                        Just (ImageOrAbbr opts) ->
                                            -- Bot badges are not meant to be used here
                                            [ Icon.imgOrAbbr [ serif, Icon.rounded20 ] opts.name opts.src, t " " ]

                                        _ ->
                                            -- EmbeddedMatters are not expected to use Octicons
                                            []

                                secondaryName =
                                    case namedEntity.secondaryName of
                                        Just sn ->
                                            [ t " ", span [ colorNote ] [ t sn ] ]

                                        Nothing ->
                                            []
                            in
                            avatar ++ [ t namedEntity.primaryName ] ++ secondaryName
                    ]

                Nothing ->
                    []

        titleBlock =
            case matter.title of
                Just (Plain "") ->
                    []

                Just (Plain plainTitle) ->
                    [ div [ prominent, Border.bot1, Border.solid ] [ t plainTitle ] ]

                Just (Markdown "") ->
                    []

                Just (Markdown mdTitle) ->
                    [ div [ prominent, Border.bot1, Border.solid ] <| markdownBlocks mdTitle ]

                Nothing ->
                    []

        permalink =
            case matter.url of
                Just url ->
                    [ div [ alignEnd ] [ ntLink [ breakWords ] { url = url, children = [ t (StringExtra.truncateUrlLikeAt30 url) ] } ] ]

                Nothing ->
                    []

        originBlock =
            case matter.origin of
                Just namedEntity ->
                    [ div [ minuscule ] <|
                        wrapInLink namedEntity.url <|
                            List.intersperse (t " ") <|
                                let
                                    originIcon =
                                        case namedEntity.avatar of
                                            Just (ImageOrAbbr opts) ->
                                                [ Icon.imgOrAbbr [ serif, Icon.rounded14 ] opts.name opts.src ]

                                            _ ->
                                                []
                                in
                                originIcon
                                    ++ [ span [ colorNote ]
                                            [ t namedEntity.primaryName
                                            , case namedEntity.secondaryName of
                                                Just sn ->
                                                    span [ colorNote ] [ t sn ]

                                                Nothing ->
                                                    none
                                            ]
                                       ]
                    ]

                Nothing ->
                    []

        ( attachedFileBlocks, mediaIndexAfterAttachedFiles ) =
            List.foldr (attachedFileBlock onMediaClick) ( [], mediaIndexAfterThumb ) matter.attachedFiles

        gutteredBlock =
            div [ flexColumn, flexBasisAuto, padding2, spacingColumn5, Border.gutter, Border.color gutterColor ] <|
                textContentsAndThumbnailBlock
                    ++ attachedFileBlocks
                    ++ permalink
                    ++ originBlock
    in
    case matter.pretext of
        Just text ->
            ( textBlocks text ++ [ gutteredBlock ] ++ accBlocks, mediaIndexAfterAttachedFiles )

        Nothing ->
            ( gutteredBlock :: accBlocks, mediaIndexAfterAttachedFiles )


attachedFileBlock : (Int -> msg) -> AttachedFile -> ( List (Html msg), Int ) -> ( List (Html msg), Int )
attachedFileBlock onMediaClick attachedFile ( accBlocks, mediaIndex ) =
    case attachedFile of
        VisualFile visualMedia ->
            ( visualMediaBlock (onMediaClick mediaIndex) visualMedia :: accBlocks, mediaIndex + 1 )

        OtherFile record ->
            let
                fileLink =
                    let
                        linkImpl linkAttrs linkLabel linkIcon url =
                            div [ nowrap, pushRight, padding5 ]
                                [ ntLink linkAttrs
                                    { url = url
                                    , children = [ t linkLabel, Image.octicon { size = prominentSize, shape = linkIcon } ]
                                    }
                                ]
                    in
                    div [ flexRow ]
                        [ p [ breakWords, flexShrink, flexBasisAuto, padding5 ] [ t record.description ]
                        , case record.fileUrl of
                            ExternalLink url ->
                                linkImpl [] "See original file " Octicons.linkExternal url

                            DownloadUrl url ->
                                linkImpl [ download "" ] "Download original file " Octicons.cloudDownload url
                        ]

                linkAndPreviewBlock =
                    case record.preview of
                        Just raw ->
                            div [ flexColumn, Border.round5, Border.w1, Border.solid ]
                                [ div [ flexBasisAuto, Border.topRound5, Background.colorSub ] [ fileLink ]
                                , pre [ minuscule, flexBasisAuto, breakWords, padding2, Border.bottomRound5, Background.colorBg ] [ t raw ]
                                ]

                        Nothing ->
                            div [ Border.round5, Border.w1, Border.solid, Background.colorSub ] [ fileLink ]
            in
            ( linkAndPreviewBlock :: accBlocks, mediaIndex )


visualMediaBlock : msg -> VisualMedia -> Html msg
visualMediaBlock onMediaClick visualMedia =
    let
        dimensionAttrs dim =
            case dim of
                Just d ->
                    if d.width > d.height then
                        -- Landscape images; respecting aspect ratio, since `height: auto` is automatically assumed.
                        [ width d.width ]

                    else
                        -- Portrait images; height is eventually contained to max-height
                        [ width d.width, height d.height ]

                Nothing ->
                    []

        badgedThumbnail badge thumb =
            withBadge
                [ flexBasisAuto
                , alignStart
                , padding2
                , Cursor.zoomIn
                , onClick onMediaClick
                ]
                { topRight = Nothing
                , bottomRight = Just <| div [ padding5, Image.fillText ] [ badge ]
                , content = thumb
                }

        imgThumb dimension description src_ =
            img ([ src src_, alt description ] ++ dimensionAttrs dimension) []
    in
    case visualMedia of
        Image record ->
            badgedThumbnail (Image.octicon { size = xProminentSize, shape = Octicons.fileMedia }) <|
                imgThumb record.dimension record.description record.src

        Video record ->
            -- Not playing Video in-column, delegate to MediaViewer
            case record.poster of
                Just src_ ->
                    badgedThumbnail (Image.octicon { size = xProminentSize, shape = Octicons.deviceCameraVideo }) <|
                        imgThumb record.dimension record.description src_

                Nothing ->
                    -- Video tag used here just for showing pseudo-thumbnail (frame at 0.5s)
                    badgedThumbnail (Image.octicon { size = xProminentSize, shape = Octicons.deviceCameraVideo }) <|
                        video
                            ([ src (record.src ++ "#t=0.5")
                             , alt record.description
                             , controls False -- Hide it, delegate to MediaViewer via onClick
                             ]
                                ++ dimensionAttrs record.dimension
                            )
                            []

        Youtube record ->
            let
                youtubeLogo64 =
                    img [ width 32, height 32, src ("data:image/png;base64," ++ youtubeLogo64Base64Data) ] []
            in
            badgedThumbnail youtubeLogo64 <|
                case record.poster of
                    Just src_ ->
                        imgThumb record.dimension "YouTube video" src_

                    Nothing ->
                        div
                            [ flexGrow
                            , flexBasisAuto
                            , alignStart
                            , flexColumn
                            , flexCenter
                            , padding15
                            , Background.colorBg
                            ]
                            [ Image.octicon { size = xxxProminentSize, shape = Octicons.deviceCameraVideo }
                            ]


youtubeLogo64Base64Data : String
youtubeLogo64Base64Data =
    String.join ""
        [ "iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFn"
        , "ZVJlYWR5ccllPAAAA25pVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/"
        , "IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6"
        , "bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTM4IDc5LjE1OTgyNCwgMjAxNi8w"
        , "OS8xNC0wMTowOTowMSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9y"
        , "Zy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIg"
        , "eG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0UmVmPSJo"
        , "dHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VSZWYjIiB4bWxuczp4bXA9Imh0"
        , "dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRp"
        , "ZDoyMDI3QzRENzE5MjA2ODExODIyQUI5Q0YwOTk5NDQ5MiIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRp"
        , "ZDpGNDNEQUM4Njg5QzQxMUU3QjFGNTgwNzI0NjY4MzExRiIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlp"
        , "ZDpGNDNEQUM4NTg5QzQxMUU3QjFGNTgwNzI0NjY4MzExRiIgeG1wOkNyZWF0b3JUb29sPSJBZG9iZSBQ"
        , "aG90b3Nob3AgQ0MgMjAxNyAoV2luZG93cykiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFu"
        , "Y2VJRD0ieG1wLmlpZDo3YjZjMTljNi0yZTYyLWZhNDQtOWY5Yy00ZmIyOTNjNGU4MmIiIHN0UmVmOmRv"
        , "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"
        ]



-- STYLES


styles : List Style
styles =
    [ s (c itemGroupClass)
        [ ( "padding-top", px itemGroupPaddingY )
        , ( "padding-bottom", px itemGroupPaddingY )
        ]
    , s (c itemGroupClass ++ ">*") [ ( "margin-left", px spacingWithinItemGroup ) ] -- Unusual margin pattern; not to be confused with spacingRow5
    , s (descOf (hov (c itemBlockClass)) (c flexHoverMenuClass)) [ ( "display", "flex" ) ]
    , s (c flexHoverMenuClass)
        [ ( "display", "none" )
        , ( "border-bottom-left-radius", "5px" )
        ]
    , s (c itemGroupContentsClass) [ ( "min-height", px minGroupContentsHeight ) ]
    , s (descOf (c itemGroupContentsClass) "img," ++ descOf (c itemGroupContentsClass) "video")
        [ ( "max-width", "100%" )
        , ( "max-height", px maxMediaHeight )
        , ( "margin-right", "auto" )
        ]
    , s (descOf (c itemGroupContentsClass) "img") [ ( "object-fit", "cover" ) ]
    , s (descOf (c itemGroupContentsClass) "video") [ ( "object-fit", "contain" ) ]
    , s (descOf (c thumbnailParentClass) "img:last-child," ++ descOf (c thumbnailParentClass) "video:last-child")
        [ ( "max-width", px maxThumbnailSize )
        , ( "max-height", px maxThumbnailSize )
        ]
    ]


itemGroupClass : String
itemGroupClass =
    "cig"


itemGroupPaddingY : Int
itemGroupPaddingY =
    5


itemGroupContentsClass : String
itemGroupContentsClass =
    "cigc"


spacingWithinItemGroup : Int
spacingWithinItemGroup =
    5


minGroupContentsHeight : Int
minGroupContentsHeight =
    -- Equals to avatar size;
    40


itemBlockClass : String
itemBlockClass =
    "cib"


flexHoverMenuClass : String
flexHoverMenuClass =
    "cihm"


maxMediaHeight : Int
maxMediaHeight =
    -- Maximum value of Slack's image thumbnail size (either width or height).
    360


thumbnailParentClass : String
thumbnailParentClass =
    "cithp"


maxThumbnailSize : Int
maxThumbnailSize =
    60
