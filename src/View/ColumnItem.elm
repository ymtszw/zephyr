module View.ColumnItem exposing (columnItemKeyEl)

import Broker exposing (Offset)
import Data.ColorTheme exposing (ColorTheme, aubergine, oneDark)
import Data.Column exposing (ColumnItem(..), Media(..))
import Data.Item exposing (Item(..), extIsImage, extIsVideo, mimeIsImage, mimeIsVideo)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Dict
import Element exposing (Color, Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Markdown.Block as Block exposing (Block, ListBlock, ListType(..))
import Markdown.Inline as Inline exposing (Inline)
import Octicons
import TextParser exposing (Parsed(..))
import Time
import TimeExtra
import Url
import View.HtmlParts exposing (..)
import View.Parts exposing (brightness, columnCodeBlockMaxHeight, columnItemAvatarSize, columnItemBorderBottom, columnWidth, cssRgba, rectElementInnerPadding, rectElementRound, scale12)
import View.TextRenderer as TextRenderer


columnItemKeyEl : ColorTheme -> Time.Zone -> List ColumnItem -> ( String, Element Msg )
columnItemKeyEl theme tz closeItems =
    -- Reverse, since we want to show closeItems in oldest to latest, opposite from other places
    case List.reverse closeItems of
        [] ->
            -- Should not happen
            ( "", Element.none )

        (item :: items) as reversed ->
            row
                [ widthFill
                , flex
                , grow
                , paddingXY 0 rectElementInnerPadding
                , style "border-bottom" <|
                    String.join " " [ "solid", px columnItemBorderBottom, cssRgba theme.bd ]
                , fontSize baseFontSize
                , fluidContainer
                ]
                [ itemAvatarEl theme item
                , itemContentsEl theme tz item items
                ]
                |> Element.html
                |> Tuple.pair (columnItemKey reversed)


baseFontSize : Int
baseFontSize =
    scale12 1


columnItemKey : List ColumnItem -> String
columnItemKey closeItems =
    closeItems
        |> List.map
            (\item ->
                case item of
                    Product offset _ ->
                        Broker.offsetToString offset

                    System id _ ->
                        id

                    LocalMessage id _ ->
                        id
            )
        |> String.join "-"


itemAvatarEl : ColorTheme -> ColumnItem -> Html Msg
itemAvatarEl theme item =
    case item of
        Product _ (DiscordItem { author }) ->
            let
                ( user, badge ) =
                    case author of
                        Discord.UserAuthor u ->
                            ( u, Nothing )

                        Discord.WebhookAuthor u ->
                            ( u, Just (botIconEl oneDark) )
            in
            iconWithBadge [ alignTop ]
                { badge = badge
                , theme = theme
                , fallback = user.username
                , url = Just <| Discord.imageUrlWithFallback (Just columnItemAvatarSize) user.discriminator user.avatar
                , size = columnItemAvatarSize
                }

        Product _ (SlackItem m) ->
            let
                ( name, url, badge ) =
                    case m.author of
                        Slack.UserAuthor u ->
                            ( Maybe.withDefault u.profile.realName u.profile.displayName
                            , Just (Url.toString u.profile.image48)
                            , Nothing
                            )

                        Slack.UserAuthorId (Slack.UserId str) ->
                            -- Slack default CDN icon require cropping, so we don't bother using
                            ( str, Nothing, Nothing )

                        Slack.BotAuthor b ->
                            ( Maybe.withDefault b.name m.username, Just (Url.toString b.icons.image48), Just (botIconEl aubergine) )

                        Slack.BotAuthorId (Slack.BotId str) ->
                            ( str, Nothing, Just (botIconEl aubergine) )
            in
            iconWithBadge [ alignTop ]
                { badge = badge
                , theme = aubergine
                , fallback = name
                , url = url
                , size = columnItemAvatarSize
                }

        System _ _ ->
            octiconAvatarEl theme Octicons.info

        LocalMessage _ _ ->
            octiconAvatarEl theme Octicons.note


botIconEl : ColorTheme -> Int -> Html Msg
botIconEl theme badgeSize =
    octicon [ bgColor (botIconBackground theme), title "BOT" ]
        { size = badgeSize, color = avatarIconFillColor theme, shape = Octicons.zap }


avatarIconFillColor : ColorTheme -> Color
avatarIconFillColor =
    .text


botIconBackground : ColorTheme -> Color
botIconBackground =
    .succ


octiconAvatarEl : ColorTheme -> (Octicons.Options -> Html.Html msg) -> Html msg
octiconAvatarEl theme shape =
    let
        octiconAvatarPadding =
            7

        octiconSize =
            columnItemAvatarSize - octiconAvatarPadding * 2
    in
    octicon
        [ width columnItemAvatarSize
        , height columnItemAvatarSize
        , padding octiconAvatarPadding
        , alignTop
        , style "border" (String.join " " [ cssRgba theme.note, "dashed", "1px" ])
        , bdRounded rectElementRound
        ]
        { size = octiconSize
        , color = avatarIconFillColor theme
        , shape = shape
        }


itemContentsEl : ColorTheme -> Time.Zone -> ColumnItem -> List ColumnItem -> Html Msg
itemContentsEl theme tz item closeItems =
    case item of
        Product offset (DiscordItem discordMessage) ->
            let
                unwrap cItem =
                    case cItem of
                        Product o (DiscordItem dm) ->
                            Just ( dm, o )

                        _ ->
                            Nothing
            in
            closeItems
                |> List.filterMap unwrap
                |> discordMessageEl tz ( discordMessage, offset )

        Product offset (SlackItem slackMessage) ->
            let
                unwrap cItem =
                    case cItem of
                        Product o (SlackItem sm) ->
                            Just ( sm, o )

                        _ ->
                            Nothing
            in
            closeItems
                |> List.filterMap unwrap
                |> slackMessageEl tz ( slackMessage, offset )

        System _ { message, mediaMaybe } ->
            lazy3 defaultItemEl theme message mediaMaybe

        LocalMessage _ { message } ->
            lazy3 defaultItemEl theme message Nothing


discordMessageEl : Time.Zone -> ( Discord.Message, Offset ) -> List ( Discord.Message, Offset ) -> Html Msg
discordMessageEl tz ( discordMessage, _ ) closeMessages =
    column [ flex, grow, alignTop ] <|
        (::) (lazy2 discordMessageHeaderEl tz discordMessage) <|
            List.map (lazy discordMessageBodyEl) <|
                (::) discordMessage <|
                    List.map Tuple.first closeMessages


discordMessageHeaderEl : Time.Zone -> Discord.Message -> Html Msg
discordMessageHeaderEl tz { author, timestamp, channelId } =
    row [ flex ]
        [ paragraph [ flex, grow, bold, fontSize userNameFontSize ]
            [ text <|
                case author of
                    Discord.UserAuthor user ->
                        user.username

                    Discord.WebhookAuthor user ->
                        user.username
            ]
        , div
            [ flex
            , fontColor (timestampFontColor oneDark)
            , fontSize baseFontSize
            ]
            [ text (TimeExtra.local tz timestamp) ]
        ]


userNameFontSize : Int
userNameFontSize =
    scale12 2


timestampFontColor : ColorTheme -> Color
timestampFontColor =
    .note


discordMessageBodyEl : Discord.Message -> Html Msg
discordMessageBodyEl m =
    let
        embeds =
            [ column [ flex, grow ] <| List.map discordEmbedEl m.embeds
            , column [ flex, grow ] <| List.map discordAttachmentEl m.attachments
            ]
    in
    column [ flex, grow ]
        (discordParagraphs maxMediaWidth m.content ++ embeds)


discordParagraphs : Int -> String -> List (Html Msg)
discordParagraphs mediaWidth text =
    -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
    TextRenderer.render
        { theme = oneDark
        , fontSize = baseFontSize
        , maxMediaWidth = mediaWidth
        , parsed = TextParser.parse Discord.parseOptions text
        }


discordEmbedEl : Discord.Embed -> Html Msg
discordEmbedEl embed =
    let
        embedDescParagraphs =
            case embed.description of
                Just desc ->
                    discordParagraphs maxEmbeddedMediaWidth desc

                Nothing ->
                    []

        embedHeaders =
            List.filterMap identity
                [ embed.author |> Maybe.map (\author -> embedAuthorEl oneDark author.url author.name author.proxyIconUrl)
                , embed.title |> Maybe.map (embedTitleEl oneDark embed.url)
                ]

        embedTexts =
            column [ flex, grow ] (embedHeaders ++ embedDescParagraphs)

        ( imagesBelowTexts, iconLikeThumb ) =
            let
                embedImageInList =
                    Maybe.withDefault [] (Maybe.map List.singleton embed.image)
            in
            case embed.thumbnail of
                Just thumb ->
                    if iconLike thumb.width thumb.height then
                        ( embedImageInList, Just thumb )

                    else
                        ( thumb :: embedImageInList, Nothing )

                Nothing ->
                    ( embedImageInList, Nothing )

        linkUrlMaybe =
            case ( embed.video, embed.url ) of
                ( Just ev, _ ) ->
                    Just ev.url

                ( _, eu ) ->
                    eu
    in
    case iconLikeThumb of
        Just thumb ->
            column (gutteredEmbedAttrs oneDark embed.color) <|
                [ row [ flex ]
                    [ embedTexts
                    , div [ flex, alignTop ] [ discordEmbedImageEl maxThumbnailSize linkUrlMaybe thumb ]
                    ]
                ]
                    ++ List.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) imagesBelowTexts

        Nothing ->
            column (gutteredEmbedAttrs oneDark embed.color) <|
                embedTexts
                    :: List.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) imagesBelowTexts


maxEmbeddedMediaWidth : Int
maxEmbeddedMediaWidth =
    maxMediaWidth - 15


embedAuthorEl : ColorTheme -> Maybe Url.Url -> String -> Maybe Url.Url -> Html Msg
embedAuthorEl theme link name icon =
    row [ bold ]
        [ wrapWithLink theme link <|
            squareIconOrHead [ flex ]
                { size = columnItemAvatarSize // 2
                , name = name
                , url = Maybe.map Url.toString icon
                }
        , paragraph [ flex, grow ] [ wrapWithLink theme link <| text name ]
        ]


wrapWithLink : ColorTheme -> Maybe Url.Url -> Html Msg -> Html Msg
wrapWithLink theme link e =
    case link of
        Just url ->
            newTabLink [ fontColor theme.link ] { url = Url.toString url, children = [ e ] }

        Nothing ->
            e


embedTitleEl : ColorTheme -> Maybe Url.Url -> String -> Html Msg
embedTitleEl theme urlMaybe title =
    paragraph [ bold ]
        [ case urlMaybe of
            Just url ->
                newTabLink [ fontColor theme.link ] { url = Url.toString url, children = [ text title ] }

            Nothing ->
                text title
        ]


iconLike : Maybe Int -> Maybe Int -> Bool
iconLike widthMaybe heightMaybe =
    let
        mapper w h =
            w == h || (w <= maxThumbnailSize && h <= maxThumbnailSize)
    in
    Maybe.map2 mapper widthMaybe heightMaybe
        |> Maybe.withDefault False


maxThumbnailSize : Int
maxThumbnailSize =
    60


gutteredEmbedAttrs : ColorTheme -> Maybe Color -> List (Attribute msg)
gutteredEmbedAttrs theme color =
    gutteredConteinerAttrs theme color
        ++ [ maxHeight maxEmbedBlockHeight
           , scrollbarY
           ]


maxEmbedBlockHeight : Int
maxEmbedBlockHeight =
    400


discordEmbedImageEl : Int -> Maybe Url.Url -> Discord.EmbedImage -> Html Msg
discordEmbedImageEl maxWidth linkUrlMaybe embedImage =
    let
        imageUrl =
            embedImage.proxyUrl
                |> Maybe.withDefault embedImage.url
                |> addDimensionQuery maxWidth embedImage.width embedImage.height
    in
    embedImageEl maxEmbeddedMediaWidth linkUrlMaybe imageUrl


embedImageEl : Int -> Maybe Url.Url -> Url.Url -> Html Msg
embedImageEl maxWidth_ link url =
    imageEl [ flex ]
        { description = "Embed Image"
        , src = url
        , maxWidth = maxWidth_
        , link = link
        }


addDimensionQuery : Int -> Maybe Int -> Maybe Int -> Url.Url -> Url.Url
addDimensionQuery maxWidth widthMaybe heightMaybe =
    Maybe.map2 (fitDimensionToWidth maxWidth) widthMaybe heightMaybe
        |> Maybe.map urlWithDimensionQuery
        |> Maybe.withDefault identity


fitDimensionToWidth : Int -> Int -> Int -> ( Int, Int )
fitDimensionToWidth maxWidth w h =
    if w <= maxWidth then
        ( w, h )

    else
        ( maxWidth, round <| toFloat h * (toFloat maxWidth / toFloat w) )


urlWithDimensionQuery : ( Int, Int ) -> Url.Url -> Url.Url
urlWithDimensionQuery ( queryWidth, queryHeight ) src =
    { src | query = Just ("width=" ++ String.fromInt queryWidth ++ "&height=" ++ String.fromInt queryHeight) }


discordAttachmentEl : Discord.Attachment -> Html Msg
discordAttachmentEl attachment =
    if extIsImage attachment.proxyUrl.path then
        imageEl []
            { src = addDimensionQuery maxMediaWidth attachment.width attachment.height attachment.proxyUrl
            , description = attachment.filename
            , link = Just attachment.url
            , maxWidth = maxMediaWidth
            }

    else if extIsVideo attachment.proxyUrl.path then
        let
            posterUrl =
                attachment.proxyUrl
                    |> addDimensionQuery maxMediaWidth attachment.width attachment.height
                    |> addPosterQuery

            addPosterQuery url =
                { url
                    | query =
                        case url.query of
                            Just q ->
                                Just ("format=jpeg&" ++ q)

                            Nothing ->
                                Just "format=jpeg"
                }
        in
        lazy2 videoEl (Just posterUrl) attachment.proxyUrl

    else
        downloadFileEl oneDark attachment.filename attachment.proxyUrl


downloadFileEl : ColorTheme -> String -> Url.Url -> Html Msg
downloadFileEl theme filename url =
    downloadLink [ flex, grow ]
        { url = Url.toString url
        , children =
            [ row
                [ padding rectElementInnerPadding
                , bgColor (brightness -1 theme.main)
                , bdRounded rectElementRound
                ]
                [ paragraph
                    [ flex
                    , grow
                    , fontSize filenameFontSize
                    , fontColor theme.link
                    ]
                    [ text filename ]
                , octicon []
                    { size = downloadIconSize
                    , color = theme.note
                    , shape = Octicons.cloudDownload
                    }
                ]
            ]
        }


filenameFontSize : Int
filenameFontSize =
    scale12 2


downloadIconSize : Int
downloadIconSize =
    20


slackMessageEl : Time.Zone -> ( Slack.Message, Offset ) -> List ( Slack.Message, Offset ) -> Html Msg
slackMessageEl tz ( m, _ ) closeMessages =
    column [ flex, grow, alignTop ] <|
        (::) (lazy2 slackMessageHeaderEl tz m) <|
            List.map (lazy slackMessageBodyEl) <|
                (m :: List.map Tuple.first closeMessages)


slackMessageHeaderEl : Time.Zone -> Slack.Message -> Html Msg
slackMessageHeaderEl tz m =
    row [ flex ]
        [ paragraph [ flex, grow, bold, fontSize userNameFontSize ]
            [ text <|
                case m.author of
                    Slack.UserAuthor user ->
                        Maybe.withDefault user.profile.realName user.profile.displayName

                    Slack.BotAuthor bot ->
                        Maybe.withDefault bot.name m.username

                    Slack.UserAuthorId (Slack.UserId str) ->
                        str

                    Slack.BotAuthorId (Slack.BotId str) ->
                        str
            ]
        , div [ flex, fontColor (timestampFontColor aubergine) ]
            [ text (TimeExtra.local tz (Slack.getPosix m)) ]
        ]


slackMessageBodyEl : Slack.Message -> Html Msg
slackMessageBodyEl m =
    let
        mainParagraphs =
            slackParagraphs maxMediaWidth m.text

        attachments =
            List.concatMap slackAttachmentEls m.attachments

        sFiles =
            List.map slackFileEl m.files
    in
    column [ flex, grow ]
        (mainParagraphs ++ attachments ++ sFiles)


slackParagraphs : Int -> String -> List (Html Msg)
slackParagraphs mediaWidth raw =
    -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
    TextRenderer.render
        { theme = aubergine
        , fontSize = baseFontSize
        , maxMediaWidth = mediaWidth
        , parsed = TextParser.parse (Slack.parseOptions Dict.empty Dict.empty) raw
        }


slackFileEl : Slack.SFile -> Html Msg
slackFileEl sf =
    let
        thumb360Url ( url, _, _ ) =
            url
    in
    if mimeIsImage sf.mimetype then
        imageEl []
            { src = Maybe.withDefault sf.url_ (Maybe.map thumb360Url sf.thumb360)
            , description = sf.name
            , maxWidth = maxMediaWidth
            , link = Just sf.url_
            }

    else if mimeIsVideo sf.mimetype then
        videoEl (Maybe.map thumb360Url sf.thumb360) sf.url_

    else if sf.mode == Slack.Snippet || sf.mode == Slack.Post then
        -- XXX we show preview of Slack Post, but Post is preformatted HTML so we actually have to render it
        column [ flex, grow ]
            [ case sf.preview of
                Just preview ->
                    codeBlock [] { theme = aubergine, maxHeight = columnCodeBlockMaxHeight, code = preview }

                Nothing ->
                    none
            , newTabLink [ fontColor aubergine.link ]
                { url = Url.toString sf.url_
                , children = [ text "View source" ]
                }
            ]

    else
        downloadFileEl aubergine sf.name sf.url_


slackAttachmentEls : Slack.Attachment -> List (Html Msg)
slackAttachmentEls a =
    case a.pretext of
        Just pretext ->
            slackParagraphs maxMediaWidth pretext ++ [ slackAttachmentBodyEl a ]

        Nothing ->
            [ slackAttachmentBodyEl a ]


slackAttachmentBodyEl : Slack.Attachment -> Html Msg
slackAttachmentBodyEl a =
    let
        headers =
            List.filterMap identity <|
                [ a.author |> Maybe.map (\author -> embedAuthorEl aubergine author.link author.name author.icon)
                , a.title |> Maybe.map (\title -> embedTitleEl aubergine title.link title.name)
                ]

        mainTexts =
            if String.isEmpty a.text then
                []

            else
                slackParagraphs maxEmbeddedMediaWidth a.text

        withImageAndGutter upperContents =
            case a.imageUrl of
                Just imageUrl ->
                    column (gutteredEmbedAttrs aubergine a.color)
                        [ row [ flex, grow ] upperContents
                        , embedImageEl maxEmbeddedMediaWidth a.imageUrl imageUrl
                        ]

                Nothing ->
                    row (gutteredEmbedAttrs aubergine a.color) upperContents
    in
    withImageAndGutter
        [ column [ flex, grow, alignTop ] <|
            case headers ++ mainTexts of
                [] ->
                    -- XXX fallback should not have formatted text so it is technically better not to parse at all
                    defaultParagraphs aubergine a.fallback

                formatted ->
                    formatted
        , case a.thumbUrl of
            Just thumbUrl ->
                div [ alignTop ] [ embedImageEl maxThumbnailSize (Maybe.andThen .link a.title) thumbUrl ]

            Nothing ->
                none
        ]


defaultItemEl : ColorTheme -> String -> Maybe Media -> Html Msg
defaultItemEl theme message mediaMaybe =
    column [ flex, grow, alignTop ] <|
        case mediaMaybe of
            Just media ->
                defaultParagraphs theme message ++ [ mediaEl media ]

            Nothing ->
                defaultParagraphs theme message


defaultParagraphs : ColorTheme -> String -> List (Html Msg)
defaultParagraphs theme message =
    if String.isEmpty message then
        []

    else
        -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
        TextRenderer.render
            { theme = theme
            , fontSize = baseFontSize
            , maxMediaWidth = maxMediaWidth
            , parsed = TextParser.parse TextParser.defaultOptions message
            }


mediaEl : Media -> Html Msg
mediaEl media =
    case media of
        Image url ->
            imageEl []
                { description = "Image"
                , src = url
                , maxWidth = maxMediaWidth
                , link = Just url
                }

        Video url ->
            videoEl Nothing url


imageEl :
    List (Attribute Msg)
    -> { description : String, src : Url.Url, maxWidth : Int, link : Maybe Url.Url }
    -> Html Msg
imageEl attrs opts =
    let
        imgAttrs =
            [ src (Url.toString opts.src)
            , alt opts.description
            , maxWidth opts.maxWidth
            ]
    in
    case opts.link of
        Just url ->
            newTabLink [] { url = Url.toString url, children = [ img (imgAttrs ++ attrs) [] ] }

        Nothing ->
            img (imgAttrs ++ attrs) []


maxMediaWidth : Int
maxMediaWidth =
    columnWidth - columnItemAvatarSize - 20


videoEl : Maybe Url.Url -> Url.Url -> Html Msg
videoEl posterMaybe url =
    video
        [ controls True
        , width maxMediaWidth
        , src (Url.toString url)
        , poster (posterMaybe |> Maybe.map Url.toString |> Maybe.withDefault "")
        ]
        [ text "Embedded video not supported."
        , a
            [ href (Url.toString url)
            , target "_blank"
            , rel "noreferrer noopener"
            ]
            [ text "[Source]" ]
        ]
