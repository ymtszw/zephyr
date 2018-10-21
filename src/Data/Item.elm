module Data.Item exposing (Item(..), decoder, encode, matchFilter)

import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord as Discord
import Element.Font
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url


type Item
    = DiscordItem Discord.Message


encode : Item -> E.Value
encode item =
    case item of
        DiscordItem discordMessage ->
            E.tagged "DiscordItem" (Discord.encodeMessage discordMessage)


decoder : Decoder Item
decoder =
    D.oneOf
        [ D.tagged "DiscordItem" DiscordItem Discord.messageDecoder
        ]


matchFilter : Item -> Filter -> Bool
matchFilter item filter =
    Filter.any (matchAtom item) filter


matchAtom : Item -> FilterAtom -> Bool
matchAtom item filterAtom =
    case ( filterAtom, item ) of
        ( ByMessage text, DiscordItem { content, embeds } ) ->
            String.contains text content || List.any (discordEmbedHasText text) embeds

        ( ByMedia filter, DiscordItem discordMessage ) ->
            discordMessageHasMedia filter discordMessage

        ( OfDiscordChannel cId, DiscordItem { channelId } ) ->
            cId == channelId

        ( RemoveMe, _ ) ->
            False


discordEmbedHasText : String -> Discord.Embed -> Bool
discordEmbedHasText text embed =
    case embed.title of
        Just title ->
            String.contains text title

        Nothing ->
            case embed.description of
                Just desc ->
                    String.contains text desc

                Nothing ->
                    case embed.url of
                        Just url ->
                            String.contains text (Url.toString url)

                        Nothing ->
                            False


discordMessageHasMedia : MediaFilter -> Discord.Message -> Bool
discordMessageHasMedia mediaFilter dm =
    case mediaFilter of
        HasImage ->
            List.any (\a -> isImageFile a.url.path) dm.attachments || List.any discordEmbedHasImage dm.embeds

        HasMovie ->
            List.any (\a -> isMovieFile a.url.path) dm.attachments || List.any discordEmbedHasMovie dm.embeds

        HasNone ->
            not <|
                List.any (\a -> isImageFile a.url.path || isMovieFile a.url.path) dm.attachments
                    || List.any (\e -> discordEmbedHasImage e || discordEmbedHasMovie e) dm.embeds


isImageFile : String -> Bool
isImageFile filename =
    String.endsWith ".jpg" filename
        || String.endsWith ".png" filename
        || String.endsWith ".gif" filename
        || String.endsWith ".webp" filename


isMovieFile : String -> Bool
isMovieFile filename =
    String.endsWith ".mp4" filename
        || String.endsWith ".ogg" filename
        || String.endsWith ".ogv" filename
        || String.endsWith ".webm" filename


discordEmbedHasImage : Discord.Embed -> Bool
discordEmbedHasImage embed =
    -- Not counting thumbnail as image
    case embed.image of
        Just _ ->
            True

        Nothing ->
            False


discordEmbedHasMovie : Discord.Embed -> Bool
discordEmbedHasMovie embed =
    case embed.video of
        Just _ ->
            True

        Nothing ->
            False
