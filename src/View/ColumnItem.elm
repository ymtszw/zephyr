module View.ColumnItem exposing (columnItemKeyEl)

import Broker exposing (Offset)
import Data.ColorTheme exposing (oneDark)
import Data.Column exposing (ColumnItem(..), Media(..))
import Data.Item exposing (Item(..), isImageFile, isMovieFile)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
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
                , paddingXY 0 5
                , spacing 5
                , BD.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
                , BD.color oneDark.bd
                ]
                [ itemAvatarEl item
                , itemContentsEl tz item items
                ]
                |> Tuple.pair (columnItemKey reversed)


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
            )
        |> String.join "-"


itemAvatarEl : ColumnItem -> Element Msg
itemAvatarEl item =
    case item of
        Product _ (DiscordItem { author }) ->
            case author of
                Discord.UserAuthor user ->
                    iconWithBadgeEl
                        { badge = Nothing
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just avatarSize) user.discriminator user.avatar
                        , size = avatarSize
                        }

                Discord.WebhookAuthor user ->
                    iconWithBadgeEl
                        { badge = Just botIconEl
                        , fallback = user.username
                        , url = Just <| Discord.imageUrlWithFallback (Just avatarSize) user.discriminator user.avatar
                        , size = avatarSize
                        }

        System _ _ ->
            iconWithBadgeEl { badge = Nothing, fallback = "Zephyr", url = Nothing, size = avatarSize }


avatarSize : Int
avatarSize =
    40


botIconEl : Element Msg
botIconEl =
    el [ padding 1, BG.color oneDark.succ, BD.rounded 2, htmlAttribute (title "BOT") ] <|
        octiconFreeSizeEl 12 Octicons.zap


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

        System _ { message, mediaMaybe } ->
            defaultItemEl message mediaMaybe


discordMessageEl : Time.Zone -> ( Discord.Message, Offset ) -> List ( Discord.Message, Offset ) -> Element Msg
discordMessageEl tz ( discordMessage, _ ) closeMessages =
    -- TODO match with official app styling
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
                , Font.size (scale12 2)
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
        , el [ alignRight, Font.color oneDark.note, Font.size (scale12 1) ] <|
            text (TimeExtra.local tz timestamp)
        ]


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
            , Font.size (scale12 1)
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
        [ wrapWithLink <| squareIconOrHeadEl (avatarSize // 2) author.name <| Maybe.map Url.toString author.proxyIconUrl
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
                        , el [ alignTop, alignRight ] <| discordEmbedImageEl maxThumbnailWidth linkUrlMaybe embedImage
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
            w == h || w <= maxThumbnailWidth
    in
    Maybe.map2 mapper widthMaybe heightMaybe
        |> Maybe.withDefault False


maxThumbnailWidth : Int
maxThumbnailWidth =
    60


discordAttachmentEl : Discord.Attachment -> Element Msg
discordAttachmentEl attachment =
    if isImageFile attachment.filename then
        newTabLink []
            { url = Url.toString attachment.url
            , label =
                imageEl attachment.filename <|
                    addDimensionQuery maxMediaWidth attachment.width attachment.height attachment.proxyUrl
            }

    else if isMovieFile attachment.filename then
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
                    , padding 5
                    , spacing 5
                    , BG.color (brightness -1 oneDark.main)
                    , BD.rounded 3
                    ]
                    [ breakP [ Font.size (scale12 2), Font.color oneDark.link ] [ breakT attachment.filename ]
                    , el [ alignRight ] <| octiconFreeSizeEl 20 Octicons.cloudDownload
                    ]
            }


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
        message
            |> Data.TextRenderer.default oneDark
            |> List.map html
            |> breakP [ Font.size (scale12 1) ]


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
    fixedColumnWidth - avatarSize - 20


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
                    ]
                    [ Html.text "[Source]" ]
                ]
