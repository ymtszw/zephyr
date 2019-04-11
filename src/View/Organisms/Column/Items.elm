module View.Organisms.Column.Items exposing (render, styles)

import Broker
import Color
import Html exposing (Attribute, Html, button, div, img, p, pre, span, video)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import List.Extra
import Octicons
import StringExtra
import TextParser
import Time
import TimeExtra exposing (ms)
import Url
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
    , onItemSourceButtonClick : String -> Int -> msg
    }


type alias Props =
    { timezone : Time.Zone
    , columnId : String
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
            , spacingRow5
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
                    children =
                        textBlocks item.body
                            ++ List.concatMap embeddedMatterBlockAndPretext item.embeddedMatters
                            ++ List.map ktBlock item.kts
                            -- TODO reactions
                            ++ List.map attachedFileBlock item.attachedFiles
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
    div [ class flexHoverMenuClass, colorNote, minuscule, padding2, spacingRow2, Background.colorMain ] <|
        case item.timestamp of
            Just posixTime ->
                [ div [ nowrap, padding5 ] [ t (TimeExtra.local props.timezone posixTime) ]
                , Icon.octiconButton [ flexItem, padding5, Image.hovText, Background.transparent, Border.round5 ]
                    { onPress = eff.onItemSourceButtonClick props.columnId item.scrollIndex
                    , size = minusculeSize
                    , shape = Octicons.code
                    }
                ]

            Nothing ->
                [ Icon.octiconButton [ flexItem, padding5, Image.hovText, Background.transparent, Border.round5 ]
                    { onPress = eff.onItemSourceButtonClick props.columnId item.scrollIndex
                    , size = minusculeSize
                    , shape = Octicons.code
                    }
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


embeddedMatterBlockAndPretext : EmbeddedMatter -> List (Html msg)
embeddedMatterBlockAndPretext matter =
    let
        wrapInLink urlMaybe children =
            case urlMaybe of
                Just url ->
                    [ ntLink [] { url = url, children = children } ]

                Nothing ->
                    children

        gutterColor =
            Maybe.withDefault Color.gray matter.color

        textContentsAndThumbnailBlock =
            case matter.thumbnail of
                Just visualMedia ->
                    [ div [ class thumbnailParentClass, flexRow, flexBasisAuto, spacingRow2 ]
                        [ div [ flexGrow ] <| authorBlock ++ titleBlock ++ textBlocks matter.body ++ List.map ktBlock matter.kts
                        , visualMediaBlock visualMedia
                        ]
                    ]

                Nothing ->
                    authorBlock ++ titleBlock ++ textBlocks matter.body ++ List.map ktBlock matter.kts

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

        gutteredBlock =
            div [ flexColumn, flexBasisAuto, padding2, spacingColumn5, Border.gutter, Border.color gutterColor ] <|
                textContentsAndThumbnailBlock
                    ++ List.map attachedFileBlock matter.attachedFiles
                    ++ permalink
                    ++ originBlock
    in
    case matter.pretext of
        Just text ->
            textBlocks text ++ [ gutteredBlock ]

        Nothing ->
            [ gutteredBlock ]


attachedFileBlock : AttachedFile -> Html msg
attachedFileBlock attachedFile =
    case attachedFile of
        VisualFile visualMedia ->
            visualMediaBlock visualMedia

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
            in
            case record.preview of
                Just raw ->
                    div [ flexColumn, Border.round5, Border.w1, Border.solid ]
                        [ div [ flexBasisAuto, Border.topRound5, Background.colorSub ] [ fileLink ]
                        , pre [ minuscule, flexBasisAuto, breakWords, padding2, Border.bottomRound5, Background.colorBg ] [ t raw ]
                        ]

                Nothing ->
                    div [ Border.round5, Border.w1, Border.solid, Background.colorSub ] [ fileLink ]


visualMediaBlock : VisualMedia -> Html msg
visualMediaBlock visualMedia =
    let
        dimensionAttrs dim =
            case dim of
                Just d ->
                    [ width d.width, height d.height ]

                Nothing ->
                    []
    in
    case visualMedia of
        Image record ->
            ntLink
                [ flexItem
                , flexBasisAuto
                , alignStart
                , padding2
                ]
                { url = record.link
                , children = [ img ([ src record.src, alt record.description ] ++ dimensionAttrs record.dimension) [] ]
                }

        Video record ->
            video
                ([ flexItem
                 , flexBasisAuto
                 , alignStart
                 , padding2
                 , controls True
                 , src record.src
                 ]
                    ++ dimensionAttrs record.dimension
                )
                [ t "Embedded video not supported. "
                , ntLink [] { url = record.link, children = [ t "[Source]" ] }
                ]



-- STYLES


styles : List Style
styles =
    [ s (c itemGroupClass)
        [ ( "padding-top", px itemGroupPaddingY )
        , ( "padding-bottom", px itemGroupPaddingY )
        ]
    , s (descOf (hov (c itemBlockClass)) (c flexHoverMenuClass)) [ ( "display", "flex" ) ]
    , s (c flexHoverMenuClass)
        [ ( "display", "none" )
        , ( "border-bottom-left-radius", "5px" )
        ]
    , s (c itemGroupContentsClass) [ ( "min-height", px minGroupContentsHeight ) ]
    , s (descOf (c itemGroupContentsClass) "img," ++ descOf (c itemGroupContentsClass) "video")
        [ ( "max-width", "100%" )
        , ( "max-height", px maxMediaHeight )
        , ( "object-fit", "cover" )
        ]
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


minGroupContentsHeight : Int
minGroupContentsHeight =
    40


itemBlockClass : String
itemBlockClass =
    "cib"


flexHoverMenuClass : String
flexHoverMenuClass =
    "cihm"


maxMediaHeight : Int
maxMediaHeight =
    400


thumbnailParentClass : String
thumbnailParentClass =
    "cithp"


maxThumbnailSize : Int
maxThumbnailSize =
    60
