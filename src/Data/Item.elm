module Data.Item exposing (Item(..), decoder, encode, isImageFile, isMovieFile, matchFilter)

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
    | SlackItem ()


encode : Item -> E.Value
encode item =
    case item of
        DiscordItem discordMessage ->
            E.tagged "DiscordItem" (Discord.encodeMessage discordMessage)

        SlackItem _ ->
            E.tag "SlackItem"


decoder : Decoder Item
decoder =
    D.oneOf
        [ D.tagged "DiscordItem" DiscordItem Discord.messageDecoder
        , D.tag "SlackItem" (SlackItem ())
        ]


matchFilter : Item -> Filter -> Bool
matchFilter item filter =
    Filter.any (matchAtom item) filter


matchAtom : Item -> FilterAtom -> Bool
matchAtom item filterAtom =
    case ( filterAtom, item ) of
        ( OfDiscordChannel cId, DiscordItem { channelId } ) ->
            cId == channelId

        ( OfDiscordChannel cId, _ ) ->
            False

        ( OfSlackConversation cId, SlackItem _ ) ->
            -- TODO
            False

        ( OfSlackConversation _, _ ) ->
            False

        ( ByMessage "", _ ) ->
            -- Short-circuit for empty query; this CAN be invalidated on input, but we are slacking
            True

        ( ByMessage text, DiscordItem { content, embeds } ) ->
            String.contains text content || List.any (discordEmbedHasText text) embeds

        ( ByMessage text, SlackItem _ ) ->
            -- TODO
            False

        ( ByMedia filter, DiscordItem discordMessage ) ->
            discordMessageHasMedia filter discordMessage

        ( ByMedia filter, SlackItem _ ) ->
            -- TODO
            False

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
    let
        lower =
            String.toLower filename
    in
    String.endsWith ".jpg" lower
        || String.endsWith ".png" lower
        || String.endsWith ".gif" lower
        || String.endsWith ".webp" lower


isMovieFile : String -> Bool
isMovieFile filename =
    let
        lower =
            String.toLower filename
    in
    -- Files that are likely playable with HTML5 <video>, which in spec does not specify supported formats
    String.endsWith ".mp4" lower
        || String.endsWith ".ogg" lower
        || String.endsWith ".ogv" lower
        || String.endsWith ".webm" lower
        || String.endsWith ".mov" lower


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
