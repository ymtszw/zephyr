module View.Organisms.Column.Items exposing (render, styles)

import Broker
import Data.ColumnItem exposing (ColumnItem)
import Data.ColumnItem.Contents exposing (..)
import Data.ColumnItem.NamedEntity exposing (Avatar(..))
import Html exposing (Attribute, Html, button, div, img, p, pre, video)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import List.Extra
import Octicons
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
import View.Style exposing (..)


type alias Effects msg =
    { scrollAttrs : List (Attribute msg) -- From Scroll.scrollAttrs; can be empty
    , onLoadMoreClick : String -> msg
    }


type alias Props =
    { timezone : Time.Zone
    , columnId : String
    , -- Expects it to be sorted from latest to oldest (globally), while reversed within each group.
      itemGroups : List ( ColumnItem, List ColumnItem )
    , hasMore : Bool
    }


render : Effects msg -> Props -> Html msg
render eff props =
    case props.itemGroups of
        [] ->
            div [ flexColumn, flexCenter, padding15, colorNote ] [ t "Waiting for messages..." ]

        itemGroups ->
            let
                attrs =
                    [ flexBasisAuto
                    , flexShrink
                    , flexColumn
                    , padding5
                    ]
                        ++ eff.scrollAttrs

                contents =
                    List.map (itemGroupKey props.timezone) itemGroups
            in
            Html.Keyed.node "div" attrs <|
                (contents ++ [ loadMoreOrButtonTokenKey (eff.onLoadMoreClick props.columnId) props.hasMore ])


loadMoreOrButtonTokenKey : msg -> Bool -> ( String, Html msg )
loadMoreOrButtonTokenKey onLoadMoreClick hasMore =
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
    Tuple.pair "loadMoreOrBottomToken" <|
        div ([ flexColumn, flexCenter, padding15 ] ++ additionalAttrs)
            [ Image.octicon
                { size = xxProminentSize
                , shape = shape
                }
            ]



-- ITEM


itemGroupKey : Time.Zone -> ( ColumnItem, List ColumnItem ) -> ( String, Html msg )
itemGroupKey tz ( oldestItem, subsequentItems ) =
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
            , itemGroupContents tz oldestItem subsequentItems
            ]


itemAuthorAvatar40 : ColumnItem -> Html msg
itemAuthorAvatar40 item =
    let
        octiconAvatar40 shape =
            Icon.octiconBlock
                [ Icon.rounded40
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
                withBadge [ badgeOutset ]
                    { topRight = Nothing
                    , bottomRight =
                        if opts.isBot then
                            Just Icon.botBadge14

                        else
                            Nothing
                    , content = Icon.imgOrAbbr [ serif, xProminent, Icon.rounded40 ] opts.name opts.src
                    }

            Nothing ->
                Icon.imgOrAbbr [ serif, xProminent, Icon.rounded40 ] item.author.primaryName Nothing


itemGroupContents : Time.Zone -> ColumnItem -> List ColumnItem -> Html msg
itemGroupContents tz oldestItem subsequentItems =
    Html.Keyed.node "div"
        [ class itemGroupContentsClass
        , clip
        , flexColumn
        , flexBasisAuto
        , flexGrow
        , flexShrink
        , spacingColumn2
        ]
        (itemGroupHeaderKey tz oldestItem :: List.map itemBlockKey (oldestItem :: subsequentItems))


itemGroupHeaderKey : Time.Zone -> ColumnItem -> ( String, Html msg )
itemGroupHeaderKey tz item =
    Tuple.pair "itemGroupHeader" <|
        div [ flexRow ]
            [ div [ flexShrink, flexBasisAuto, breakWords, bold, prominent ] [ t item.author.primaryName ]
            , case item.author.secondaryName of
                Just secondaryName ->
                    div [ colorNote, flexShrink, flexBasisAuto, breakWords ] [ t secondaryName ]

                Nothing ->
                    none
            , case item.timestamp of
                Just posixTime ->
                    div [ colorNote, pushRight, flexBasisAuto ] [ t (TimeExtra.local tz posixTime) ]

                Nothing ->
                    none
            ]


itemBlockKey : ColumnItem -> ( String, Html msg )
itemBlockKey item =
    let
        attachedFileBlocks =
            List.map attachedFileBlock item.attachedFiles
    in
    Tuple.pair item.id <|
        div [ flexColumn, flexBasisAuto, flexShrink, flexGrow, padding2, spacingColumn2 ] <|
            bodyBlocks item.body
                -- TODO embeddedBlocks
                -- TODO KTS
                -- TODO reactions
                ++ attachedFileBlocks


bodyBlocks : Text -> List (Html msg)
bodyBlocks text =
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


attachedFileBlock : AttachedFile -> Html msg
attachedFileBlock attachedFile =
    let
        dimensionAttrs dim =
            case dim of
                Just ( w, h ) ->
                    [ width w, height h ]

                Nothing ->
                    []
    in
    case attachedFile of
        VisualFile (Image record) ->
            ntLink
                [ flexItem
                , flexBasisAuto
                , alignStart
                ]
                { url = record.src
                , children = [ img ([ src record.src, alt record.description ] ++ dimensionAttrs record.dimension) [] ]
                }

        VisualFile (Video record) ->
            video
                ([ flexItem
                 , flexBasisAuto
                 , alignStart
                 , controls True
                 , src record.src
                 ]
                    ++ dimensionAttrs record.dimension
                )
                [ t "Embedded video not supported. "
                , ntLink [] { url = record.src, children = [ t "[Source]" ] }
                ]

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
                        , pre [ flexBasisAuto, breakWords, padding2, Border.bottomRound5, Background.colorBg ] [ t raw ]
                        ]

                Nothing ->
                    div [ Border.round5, Border.w1, Border.solid, Background.colorSub ] [ fileLink ]



-- STYLES


styles : List Style
styles =
    [ s (c itemGroupClass)
        [ ( "padding-top", px itemGroupPaddingY )
        , ( "padding-bottom", px itemGroupPaddingY )
        ]
    , s (descOf (c itemGroupContentsClass) "img," ++ descOf (c itemGroupContentsClass) "video")
        [ ( "max-width", "100%" )
        , ( "max-height", px maxMediaHeight )
        , ( "object-fit", "cover" )
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


maxMediaHeight : Int
maxMediaHeight =
    400
