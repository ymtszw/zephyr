module View.ColumnItem exposing (columnItemKeyEl)

import Broker exposing (Offset)
import Data.ColorTheme exposing (oneDark)
import Data.Column exposing (ColumnItem(..), Media(..))
import Data.Item exposing (Item(..), extIsImage, extIsVideo)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Data.TextRenderer
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Lazy exposing (lazy2)
import Html
import Html.Attributes exposing (title)
import Octicons
import Time
import TimeExtra
import Url
import View.Parts exposing (..)


columnItemKeyEl : Time.Zone -> List ColumnItem -> ( String, Element Msg )
columnItemKeyEl tz closeItems =
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
                , BD.color oneDark.bd
                , Font.size baseFontSize
                ]
                [ itemAvatarEl item
                , itemContentsEl tz item items
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


itemAvatarEl : ColumnItem -> Element Msg
itemAvatarEl item =
    case item of
        Product _ (DiscordItem { author }) ->
            case author of
                Discord.UserAuthor user ->
                    iconWithBadgeEl [ alignTop ]
                        { badge = Nothing
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just columnItemAvatarSize) user.discriminator user.avatar
                        , size = columnItemAvatarSize
                        }

                Discord.WebhookAuthor user ->
                    iconWithBadgeEl [ alignTop ]
                        { badge = Just botIconEl
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just columnItemAvatarSize) user.discriminator user.avatar
                        , size = columnItemAvatarSize
                        }

        Product _ (SlackItem _) ->
            iconWithBadgeEl [ alignTop ]
                { badge = Nothing
                , fallback = "Slack"
                , url = Just <| Slack.defaultIconUrl (Just columnItemAvatarSize)
                , size = columnItemAvatarSize
                }

        System _ _ ->
            octiconAvatarEl Octicons.info

        LocalMessage _ _ ->
            octiconAvatarEl Octicons.note


botIconEl : Int -> Element Msg
botIconEl badgeSize =
    octiconEl [ BG.color botIconBackground, htmlAttribute (title "BOT") ]
        { size = badgeSize, color = avatarIconFillColor, shape = Octicons.zap }


avatarIconFillColor : Color
avatarIconFillColor =
    oneDark.text


botIconBackground : Color
botIconBackground =
    oneDark.succ


octiconAvatarEl : (Octicons.Options -> Html.Html msg) -> Element msg
octiconAvatarEl shape =
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
        , BD.color oneDark.note
        , BD.dashed
        , BD.width 1
        , BD.rounded rectElementRound
        ]
        { size = octiconSize
        , color = avatarIconFillColor
        , shape = shape
        }


itemContentsEl : Time.Zone -> ColumnItem -> List ColumnItem -> Element Msg
itemContentsEl tz item closeItems =
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
            defaultItemEl message mediaMaybe

        LocalMessage _ { message } ->
            defaultItemEl message Nothing


discordMessageEl : Time.Zone -> ( Discord.Message, Offset ) -> List ( Discord.Message, Offset ) -> Element Msg
discordMessageEl tz ( discordMessage, _ ) closeMessages =
    column [ width fill, spacing 5, alignTop ] <|
        (::) (discordMessageHeaderEl tz discordMessage) <|
            List.map discordMessageBodyEl <|
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
        , el [ alignRight, Font.color timestampFontColor ] <|
            text (TimeExtra.local tz timestamp)
        ]


userNameFontSize : Int
userNameFontSize =
    scale12 2


timestampFontColor : Color
timestampFontColor =
    oneDark.note


discordMessageBodyEl : Discord.Message -> Element Msg
discordMessageBodyEl discordMessage =
    column [ spacingXY 0 5, width fill ]
        [ messageToParagraph discordMessage.content
        , collapsingColumn [ width fill, spacing 5 ] <| List.map discordEmbedEl discordMessage.embeds
        , collapsingColumn [ width fill, spacing 5 ] <| List.map discordAttachmentEl discordMessage.attachments
        ]


discordEmbedEl : Discord.Embed -> Element Msg
discordEmbedEl embed =
    [ embed.author |> Maybe.map discordEmbedAuthorEl
    , embed.title |> Maybe.map (discordEmbedTitleEl embed.url)
    , embed.description |> Maybe.map messageToParagraph
    ]
        |> List.filterMap identity
        |> breakTColumn
            [ width fill
            , spacing 5
            ]
        |> discordSmartThumbnailEl embed


maxEmbeddedMediaWidth : Int
maxEmbeddedMediaWidth =
    maxMediaWidth - 15


discordEmbedAuthorEl : Discord.EmbedAuthor -> Element Msg
discordEmbedAuthorEl author =
    let
        wrapWithLink element =
            case author.url of
                Just url ->
                    newTabLink [] { url = Url.toString url, label = element }

                Nothing ->
                    element
    in
    row [ spacing 5, Font.bold ]
        [ wrapWithLink <|
            squareIconOrHeadEl []
                { size = columnItemAvatarSize // 2
                , name = author.name
                , url = Maybe.map Url.toString author.proxyIconUrl
                }
        , paragraph [] [ wrapWithLink <| text author.name ]
        ]


discordEmbedTitleEl : Maybe Url.Url -> String -> Element Msg
discordEmbedTitleEl urlMaybe title =
    paragraph [ Font.color oneDark.link ]
        [ case urlMaybe of
            Just url ->
                newTabLink [] { url = Url.toString url, label = text title }

            Nothing ->
                text title
        ]


discordEmbedImageEl : Int -> Maybe Url.Url -> Discord.EmbedImage -> Element Msg
discordEmbedImageEl maxWidth linkUrlMaybe embedImage =
    newTabLink []
        { url = Url.toString (Maybe.withDefault embedImage.url linkUrlMaybe)
        , label =
            image [ width (shrink |> maximum maxWidth) ]
                { src =
                    embedImage.proxyUrl
                        |> Maybe.withDefault embedImage.url
                        |> addDimensionQuery maxWidth embedImage.width embedImage.height
                        |> Url.toString
                , description = "Thumbnail"
                }
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


discordSmartThumbnailEl : Discord.Embed -> Element Msg -> Element Msg
discordSmartThumbnailEl embed element =
    let
        wrapperAttrs =
            [ padding 5
            , spacing 5
            , BG.color (brightness -1 oneDark.main)
            , BD.color (Maybe.withDefault oneDark.bg embed.color)
            , BD.widthEach { left = 4, top = 0, right = 0, bottom = 0 }
            , BD.rounded 3
            ]

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
                column wrapperAttrs
                    [ row [ width fill, spacing 5 ]
                        [ element
                        , el [ alignTop, alignRight ] <| discordEmbedImageEl maxThumbnailSize linkUrlMaybe embedImage
                        ]
                    , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) |> Maybe.withDefault none
                    ]

            else
                column wrapperAttrs
                    [ element
                    , el [ alignLeft ] <| discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe embedImage
                    , embed.image |> Maybe.map (discordEmbedImageEl maxEmbeddedMediaWidth linkUrlMaybe) |> Maybe.withDefault none
                    ]

        Nothing ->
            column wrapperAttrs
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


discordAttachmentEl : Discord.Attachment -> Element Msg
discordAttachmentEl attachment =
    if extIsImage attachment.proxyUrl.path then
        newTabLink []
            { url = Url.toString attachment.url
            , label =
                imageEl attachment.filename <|
                    addDimensionQuery maxMediaWidth attachment.width attachment.height attachment.proxyUrl
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
        download [ width fill ]
            { url = Url.toString attachment.proxyUrl
            , label =
                row
                    [ width fill
                    , padding rectElementInnerPadding
                    , spacing spacingUnit
                    , BG.color (brightness -1 oneDark.main)
                    , BD.rounded rectElementRound
                    ]
                    [ breakP
                        [ Font.size attachmentFilenameFontSize
                        , Font.color attachmentFilenameColor
                        ]
                        [ breakT attachment.filename ]
                    , octiconEl [ alignRight ]
                        { size = downloadIconSize
                        , color = defaultOcticonColor
                        , shape = Octicons.cloudDownload
                        }
                    ]
            }


attachmentFilenameFontSize : Int
attachmentFilenameFontSize =
    scale12 2


attachmentFilenameColor : Color
attachmentFilenameColor =
    oneDark.link


downloadIconSize : Int
downloadIconSize =
    20


slackMessageEl : Time.Zone -> ( Slack.Message, Offset ) -> List ( Slack.Message, Offset ) -> Element Msg
slackMessageEl tz ( m, _ ) closeMessages =
    column [ width fill, spacing 5, alignTop ]
        [ --TODO
          text "Slack message"
        ]


defaultItemEl : String -> Maybe Media -> Element Msg
defaultItemEl message mediaMaybe =
    case mediaMaybe of
        Just media ->
            textColumn [ spacingXY 0 10, width fill, alignTop ]
                [ messageToParagraph message
                , mediaEl media
                ]

        Nothing ->
            el [ width fill, alignTop ] (messageToParagraph message)


messageToParagraph : String -> Element Msg
messageToParagraph message =
    if String.isEmpty message then
        none

    else
        -- TODO consider storing parsed result, rather than parsing every time. https://github.com/ymtszw/zephyr/issues/23
        message
            |> Data.TextRenderer.default oneDark
            |> List.map html
            |> breakP []


mediaEl : Media -> Element Msg
mediaEl media =
    case media of
        Image url ->
            imageEl "Image" url

        Movie url ->
            videoEl Nothing url


imageEl : String -> Url.Url -> Element Msg
imageEl desc url =
    image [ width (fill |> maximum maxMediaWidth) ] { src = Url.toString url, description = desc }


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
