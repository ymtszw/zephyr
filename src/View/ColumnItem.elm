module View.ColumnItem exposing (columnItemKeyEl)

import Broker exposing (Offset)
import Data.ColorTheme exposing (ColorTheme, aubergine, oneDark)
import Data.Column exposing (ColumnItem(..), Media(..))
import Data.Item exposing (Item(..), extIsImage, extIsVideo, mimeIsImage, mimeIsVideo)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Dict
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Lazy exposing (..)
import Html
import Html.Attributes exposing (title)
import Markdown.Block as Block exposing (Block, ListBlock, ListType(..))
import Markdown.Inline as Inline exposing (Inline)
import Octicons
import TextParser exposing (Parsed(..))
import Time
import TimeExtra
import Url
import View.Parts exposing (..)
import View.TextRenderer as TextRenderer


columnItemKeyEl : ColorTheme -> Time.Zone -> List ColumnItem -> ( String, Element Msg )
columnItemKeyEl theme tz closeItems =
    -- Reverse, since we want to show closeItems in oldest to latest, opposite from other places
    case List.reverse closeItems of
        [] ->
            -- Should not happen
            ( "", none )

        (item :: items) as reversed ->
            row
                [ width fill
                , paddingXY 0 rectElementInnerPadding
                , spacing spacingUnit
                , BD.widthEach { top = 0, bottom = columnItemBorderBottom, left = 0, right = 0 }
                , BD.color theme.bd
                , Font.size baseFontSize
                , fluidContainer
                ]
                [ itemAvatarEl theme item
                , itemContentsEl theme tz item items
                ]
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


itemAvatarEl : ColorTheme -> ColumnItem -> Element Msg
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
            iconWithBadgeEl [ alignTop ]
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
            iconWithBadgeEl [ alignTop ]
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


botIconEl : ColorTheme -> Int -> Element Msg
botIconEl theme badgeSize =
    octiconEl [ BG.color (botIconBackground theme), htmlAttribute (title "BOT") ]
        { size = badgeSize, color = avatarIconFillColor theme, shape = Octicons.zap }


avatarIconFillColor : ColorTheme -> Color
avatarIconFillColor =
    .text


botIconBackground : ColorTheme -> Color
botIconBackground =
    .succ


octiconAvatarEl : ColorTheme -> (Octicons.Options -> Html.Html msg) -> Element msg
octiconAvatarEl theme shape =
    let
        octiconAvatarPadding =
            7

        octiconSize =
            columnItemAvatarSize - octiconAvatarPadding * 2
    in
    octiconEl
        [ width (px columnItemAvatarSize)
        , height (px columnItemAvatarSize)
        , padding octiconAvatarPadding
        , alignTop
        , BD.color theme.note
        , BD.dashed
        , BD.width 1
        , BD.rounded rectElementRound
        ]
        { size = octiconSize
        , color = avatarIconFillColor theme
        , shape = shape
        }


itemContentsEl : ColorTheme -> Time.Zone -> ColumnItem -> List ColumnItem -> Element Msg
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


discordMessageEl : Time.Zone -> ( Discord.Message, Offset ) -> List ( Discord.Message, Offset ) -> Element Msg
discordMessageEl tz ( discordMessage, _ ) closeMessages =
    column [ width fill, spacing 5, alignTop ] <|
        (::) (lazy2 discordMessageHeaderEl tz discordMessage) <|
            List.map (lazy discordMessageBodyEl) <|
                (::) discordMessage <|
                    List.map Tuple.first closeMessages


discordMessageHeaderEl : Time.Zone -> Discord.Message -> Element Msg
discordMessageHeaderEl tz { author, timestamp, channelId } =
    let
        userNameEl =
            breakP
                [ alignLeft
                , Font.bold
                , Font.size userNameFontSize
                ]
                [ breakT <|
                    case author of
                        Discord.UserAuthor user ->
                            user.username

                        Discord.WebhookAuthor user ->
                            user.username
                ]
    in
    row [ width fill, spacing 5 ]
        [ userNameEl
        , el [ alignRight, Font.color (timestampFontColor oneDark) ] <|
            text (TimeExtra.local tz timestamp)
        ]


userNameFontSize : Int
userNameFontSize =
    scale12 2


timestampFontColor : ColorTheme -> Color
timestampFontColor =
    .note


discordMessageBodyEl : Discord.Message -> Element Msg
discordMessageBodyEl m =
    let
        embeds =
            [ collapsingColumn [ width fill, spacing spacingUnit ] <| List.map discordEmbedEl m.embeds
            , collapsingColumn [ width fill, spacing spacingUnit ] <| List.map discordAttachmentEl m.attachments
            ]
    in
    column
        [ width fill
        , spacing spacingUnit
        , mouseOver [ BG.color oneDark.sub ]
        ]
        (discordParagraphs maxMediaWidth m.content ++ embeds)


discordParagraphs : Int -> String -> List (Element Msg)
discordParagraphs mediaWidth text =
    -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
    TextRenderer.render
        { theme = oneDark
        , fontSize = baseFontSize
        , maxMediaWidth = mediaWidth
        , parsed = TextParser.parse Discord.parseOptions text
        }


discordEmbedEl : Discord.Embed -> Element Msg
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
                [ embed.author |> Maybe.map (\author -> embedAuthorEl author.url author.name author.proxyIconUrl)
                , embed.title |> Maybe.map (embedTitleEl oneDark embed.url)
                ]
    in
    discordSmartThumbnailEl embed <|
        breakTColumn [ width fill, spacing spacingUnit ] <|
            (embedHeaders ++ embedDescParagraphs)


maxEmbeddedMediaWidth : Int
maxEmbeddedMediaWidth =
    maxMediaWidth - 15


embedAuthorEl : Maybe Url.Url -> String -> Maybe Url.Url -> Element Msg
embedAuthorEl link name icon =
    row [ spacing spacingUnit, Font.bold ]
        [ wrapWithLink link <|
            squareIconOrHeadEl []
                { size = columnItemAvatarSize // 2
                , name = name
                , url = Maybe.map Url.toString icon
                }
        , paragraph [] [ wrapWithLink link <| text name ]
        ]


wrapWithLink : Maybe Url.Url -> Element Msg -> Element Msg
wrapWithLink link e =
    case link of
        Just url ->
            newTabLink [] { url = Url.toString url, label = e }

        Nothing ->
            e


embedTitleEl : ColorTheme -> Maybe Url.Url -> String -> Element Msg
embedTitleEl theme urlMaybe title =
    paragraph [ Font.bold ]
        [ case urlMaybe of
            Just url ->
                newTabLink [ Font.color theme.link ] { url = Url.toString url, label = text title }

            Nothing ->
                text title
        ]


discordSmartThumbnailEl : Discord.Embed -> Element Msg -> Element Msg
discordSmartThumbnailEl embed element =
    let
        linkUrlMaybe =
            case ( embed.video, embed.url ) of
                ( Just ev, _ ) ->
                    Just ev.url

                ( _, eu ) ->
                    eu
    in
    case embed.thumbnail of
        Just embedImage ->
            -- Assuming embed.video always comes with embed.thumbnail
            -- TODO load embedded players on click
            if iconLike embedImage.width embedImage.height then
                column (gutteredEmbedAttrs oneDark embed.color)
                    [ row [ width fill, spacing 5 ]
                        [ element
                        , el [ alignTop, alignRight ] <| discordEmbedImageEl maxThumbnailSize linkUrlMaybe embedImage
                        ]
                    , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) |> Maybe.withDefault none
                    ]

            else
                column (gutteredEmbedAttrs oneDark embed.color)
                    [ element
                    , el [ alignLeft ] <| discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe embedImage
                    , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) |> Maybe.withDefault none
                    ]

        Nothing ->
            column (gutteredEmbedAttrs oneDark embed.color)
                [ element
                , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) |> Maybe.withDefault none
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
        ++ [ height (shrink |> maximum maxEmbedBlockHeight)
           , scrollbarY
           ]


maxEmbedBlockHeight : Int
maxEmbedBlockHeight =
    400


discordEmbedImageEl : Int -> Maybe Url.Url -> Discord.EmbedImage -> Element Msg
discordEmbedImageEl maxWidth linkUrlMaybe embedImage =
    let
        imageUrl =
            embedImage.proxyUrl
                |> Maybe.withDefault embedImage.url
                |> addDimensionQuery maxWidth embedImage.width embedImage.height
    in
    embedImageEl maxWidth linkUrlMaybe imageUrl


embedImageEl : Int -> Maybe Url.Url -> Url.Url -> Element Msg
embedImageEl maxWidth linkMaybe imageUrl =
    wrapWithLink linkMaybe <|
        image [ width (shrink |> maximum maxWidth) ]
            { src = Url.toString imageUrl
            , description = "Embedded Image"
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


discordAttachmentEl : Discord.Attachment -> Element Msg
discordAttachmentEl attachment =
    if extIsImage attachment.proxyUrl.path then
        imageEl
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


downloadFileEl : ColorTheme -> String -> Url.Url -> Element Msg
downloadFileEl theme filename url =
    download [ width fill ]
        { url = Url.toString url
        , label =
            row
                [ width fill
                , padding rectElementInnerPadding
                , spacing spacingUnit
                , BG.color (brightness -1 theme.main)
                , BD.rounded rectElementRound
                ]
                [ breakP
                    [ Font.size filenameFontSize
                    , Font.color theme.link
                    ]
                    [ breakT filename ]
                , octiconEl [ alignRight ]
                    { size = downloadIconSize
                    , color = theme.note
                    , shape = Octicons.cloudDownload
                    }
                ]
        }


filenameFontSize : Int
filenameFontSize =
    scale12 2


downloadIconSize : Int
downloadIconSize =
    20


slackMessageEl : Time.Zone -> ( Slack.Message, Offset ) -> List ( Slack.Message, Offset ) -> Element Msg
slackMessageEl tz ( m, _ ) closeMessages =
    column [ width fill, spacing spacingUnit, alignTop ] <|
        (::) (lazy2 slackMessageHeaderEl tz m) <|
            List.map (lazy slackMessageBodyEl) <|
                (m :: List.map Tuple.first closeMessages)


slackMessageHeaderEl : Time.Zone -> Slack.Message -> Element Msg
slackMessageHeaderEl tz m =
    let
        userNameEl =
            breakP
                [ alignLeft
                , Font.bold
                , Font.size userNameFontSize
                ]
                [ breakT <|
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
    in
    row [ width fill, spacing spacingUnit ]
        [ userNameEl
        , el [ alignRight, Font.color (timestampFontColor aubergine) ] <|
            text (TimeExtra.local tz (Slack.getPosix m))
        ]


slackMessageBodyEl : Slack.Message -> Element Msg
slackMessageBodyEl m =
    let
        mainParagraphs =
            slackParagraphs maxMediaWidth m.text

        attachments =
            List.concatMap slackAttachmentEls m.attachments

        sFiles =
            List.map slackFileEl m.files
    in
    collapsingColumn
        [ width fill
        , spacing spacingUnit
        , mouseOver [ BG.color aubergine.sub ]
        ]
        (mainParagraphs ++ attachments ++ sFiles)


slackParagraphs : Int -> String -> List (Element Msg)
slackParagraphs mediaWidth raw =
    -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
    TextRenderer.render
        { theme = aubergine
        , fontSize = baseFontSize
        , maxMediaWidth = mediaWidth
        , parsed = TextParser.parse (Slack.parseOptions Dict.empty Dict.empty) raw
        }


slackFileEl : Slack.SFile -> Element Msg
slackFileEl sf =
    let
        thumb360Url ( url, _, _ ) =
            url
    in
    if mimeIsImage sf.mimetype then
        imageEl
            { src = Maybe.withDefault sf.url_ (Maybe.map thumb360Url sf.thumb360)
            , description = sf.name
            , maxWidth = maxMediaWidth
            , link = Just sf.url_
            }

    else if mimeIsVideo sf.mimetype then
        videoEl (Maybe.map thumb360Url sf.thumb360) sf.url_

    else if sf.mode == Slack.Snippet || sf.mode == Slack.Post then
        -- XXX we show preview of Slack Post, but Post is preformatted HTML so we actually have to render it
        column [ width fill, spacing spacingUnit ]
            [ case sf.preview of
                Just preview ->
                    codeBlock [] { theme = aubergine, maxHeight = columnCodeBlockMaxHeight, code = preview }

                Nothing ->
                    none
            , newTabLink [ Font.color aubergine.link ] { url = Url.toString sf.url_, label = text "View source" }
            ]

    else
        downloadFileEl aubergine sf.name sf.url_


slackAttachmentEls : Slack.Attachment -> List (Element Msg)
slackAttachmentEls a =
    case a.pretext of
        Just pretext ->
            slackParagraphs maxMediaWidth pretext ++ [ slackAttachmentBodyEl a ]

        Nothing ->
            [ slackAttachmentBodyEl a ]


slackAttachmentBodyEl : Slack.Attachment -> Element Msg
slackAttachmentBodyEl a =
    let
        headers =
            List.filterMap identity <|
                [ a.author |> Maybe.map (\author -> embedAuthorEl author.link author.name author.icon)
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
                        [ row [ width fill, spacing spacingUnit ] upperContents
                        , embedImageEl maxEmbeddedMediaWidth a.imageUrl imageUrl
                        ]

                Nothing ->
                    row (gutteredEmbedAttrs aubergine a.color) upperContents
    in
    withImageAndGutter
        [ column [ width fill, spacing spacingUnit, alignTop ] <|
            case headers ++ mainTexts of
                [] ->
                    -- XXX fallback should not have formatted text so it is technically better not to parse at all
                    defaultParagraphs aubergine a.fallback

                formatted ->
                    formatted
        , case a.thumbUrl of
            Just thumbUrl ->
                el [ alignTop ] <| embedImageEl maxThumbnailSize (Maybe.andThen .link a.title) thumbUrl

            Nothing ->
                none
        ]


defaultItemEl : ColorTheme -> String -> Maybe Media -> Element Msg
defaultItemEl theme message mediaMaybe =
    column [ width fill, spacing spacingUnit, alignTop, mouseOver [ BG.color theme.sub ] ] <|
        case mediaMaybe of
            Just media ->
                defaultParagraphs theme message ++ [ mediaEl media ]

            Nothing ->
                defaultParagraphs theme message


defaultParagraphs : ColorTheme -> String -> List (Element Msg)
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


mediaEl : Media -> Element Msg
mediaEl media =
    case media of
        Image url ->
            imageEl
                { description = "Image"
                , src = url
                , maxWidth = maxMediaWidth
                , link = Just url
                }

        Video url ->
            videoEl Nothing url


imageEl : { description : String, src : Url.Url, maxWidth : Int, link : Maybe Url.Url } -> Element Msg
imageEl opts =
    wrapWithLink opts.link <|
        image [ width (fill |> maximum opts.maxWidth) ]
            { src = Url.toString opts.src, description = opts.description }


maxMediaWidth : Int
maxMediaWidth =
    columnWidth - columnItemAvatarSize - 20


videoEl : Maybe Url.Url -> Url.Url -> Element Msg
videoEl posterMaybe url =
    el [ width fill, centerX ] <|
        html <|
            Html.video
                [ Html.Attributes.controls True
                , Html.Attributes.width maxMediaWidth
                , Html.Attributes.src (Url.toString url)
                , Html.Attributes.poster (posterMaybe |> Maybe.map Url.toString |> Maybe.withDefault "")
                ]
                [ Html.text "Embedded video not supported."
                , Html.a
                    [ Html.Attributes.href (Url.toString url)
                    , Html.Attributes.target "_blank"
                    , Html.Attributes.rel "noreferrer noopener"
                    ]
                    [ Html.text "[Source]" ]
                ]
