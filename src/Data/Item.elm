module Data.Item exposing (Item(..), decoder, encode, isImageFile, isMovieFile, matchFilter)

import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Element.Font
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import StringExtra
import Url


type Item
    = DiscordItem Discord.Message
    | SlackItem Slack.Message


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
        , D.tagged "SlackItem" SlackItem Slack.messageDecoder
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

        ( ByMessage text, DiscordItem { content, author, embeds } ) ->
            StringExtra.containsCaseIgnored text content
                || discordAuthorHasText text author
                || List.any (discordEmbedHasText text) embeds

        ( ByMessage text, SlackItem m ) ->
            StringExtra.containsCaseIgnored text m.text
                || List.any (slackAttachmentHasText text) m.attachments

        ( ByMedia filter, DiscordItem discordMessage ) ->
            discordMessageHasMedia filter discordMessage

        ( ByMedia filter, SlackItem _ ) ->
            -- TODO
            False

        ( RemoveMe, _ ) ->
            False


discordAuthorHasText : String -> Discord.Author -> Bool
discordAuthorHasText text author =
    case author of
        Discord.UserAuthor user ->
            StringExtra.containsCaseIgnored text user.username

        Discord.WebhookAuthor user ->
            StringExtra.containsCaseIgnored text user.username


discordEmbedHasText : String -> Discord.Embed -> Bool
discordEmbedHasText text embed =
    checkMaybeField text embed.title
        || checkMaybeField text embed.description
        || checkMaybeField text (Maybe.map .name embed.author)


checkMaybeField : String -> Maybe String -> Bool
checkMaybeField text stringMaybe =
    case stringMaybe of
        Just string ->
            StringExtra.containsCaseIgnored text string

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


slackAttachmentHasText : String -> Slack.Attachment -> Bool
slackAttachmentHasText text a =
    checkMaybeField text a.pretext
        || checkMaybeField text (Maybe.map .name a.author)
        || checkMaybeField text (Maybe.map .name a.title)
        || StringExtra.containsCaseIgnored text a.text
        || StringExtra.containsCaseIgnored text a.fallback
