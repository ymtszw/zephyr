module Data.Item exposing (Item(..), decoder, encode, extIsImage, extIsVideo, matchFilter, mimeIsImage, mimeIsVideo)

import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack.Message as SlackMessage
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import StringExtra


type Item
    = DiscordItem Discord.Message
    | SlackItem SlackMessage.Message


encode : Item -> E.Value
encode item =
    case item of
        DiscordItem discordMessage ->
            E.tagged "DiscordItem" (Discord.encodeMessage discordMessage)

        SlackItem slackMessage ->
            E.tagged "SlackItem" (SlackMessage.encode slackMessage)


decoder : Decoder Item
decoder =
    D.oneOf
        [ D.tagged "DiscordItem" DiscordItem Discord.messageDecoder
        , D.tagged "SlackItem" SlackItem SlackMessage.decoder
        ]


matchFilter : Item -> Filter -> Bool
matchFilter item filter =
    Filter.any (matchAtom item) filter


matchAtom : Item -> FilterAtom -> Bool
matchAtom item filterAtom =
    case ( filterAtom, item ) of
        ( OfDiscordChannel cId, DiscordItem { channelId } ) ->
            cId == channelId

        ( OfDiscordChannel _, _ ) ->
            False

        ( OfSlackConversation cId, SlackItem sMessage ) ->
            cId == SlackMessage.getConvoId sMessage

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
            StringExtra.containsCaseIgnored text (SlackMessage.getText m)
                || List.any (slackAttachmentHasText text) (SlackMessage.getAttachments m)
                || List.any (slackFileHasText text) (SlackMessage.getFiles m)

        ( ByMedia filter, DiscordItem discordMessage ) ->
            discordMessageHasMedia filter discordMessage

        ( ByMedia filter, SlackItem m ) ->
            slackMessageHasMedia filter m

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
            List.any (\a -> extIsImage a.url.path) dm.attachments || List.any discordEmbedHasImage dm.embeds

        HasVideo ->
            List.any (\a -> extIsVideo a.url.path) dm.attachments || List.any discordEmbedHasVideo dm.embeds

        HasNone ->
            not <|
                List.any (\a -> extIsImage a.url.path || extIsVideo a.url.path) dm.attachments
                    || List.any (\e -> discordEmbedHasImage e || discordEmbedHasVideo e) dm.embeds


extIsImage : String -> Bool
extIsImage filename =
    let
        lower =
            String.toLower filename
    in
    String.endsWith ".jpg" lower
        || String.endsWith ".png" lower
        || String.endsWith ".gif" lower
        || String.endsWith ".webp" lower


extIsVideo : String -> Bool
extIsVideo filename =
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


discordEmbedHasVideo : Discord.Embed -> Bool
discordEmbedHasVideo embed =
    case embed.video of
        Just _ ->
            True

        Nothing ->
            False


slackAttachmentHasText : String -> SlackMessage.Attachment -> Bool
slackAttachmentHasText text a =
    checkMaybeField text a.pretext
        || checkMaybeField text (Maybe.map .name a.author)
        || checkMaybeField text (Maybe.map .name a.title)
        || StringExtra.containsCaseIgnored text a.text
        || StringExtra.containsCaseIgnored text a.fallback


slackFileHasText : String -> SlackMessage.SFile -> Bool
slackFileHasText text sf =
    case sf.preview of
        Just preview ->
            StringExtra.containsCaseIgnored text preview

        Nothing ->
            False


slackMessageHasMedia : MediaFilter -> SlackMessage.Message -> Bool
slackMessageHasMedia mediaFilter m =
    case mediaFilter of
        HasImage ->
            List.any (.mimetype >> mimeIsImage) (SlackMessage.getFiles m)
                || List.any slackAttachmentHasImage (SlackMessage.getAttachments m)

        HasVideo ->
            -- TODO Slack also has video_* fields but not yet supported
            List.any (.mimetype >> mimeIsVideo) (SlackMessage.getFiles m)

        HasNone ->
            not <|
                List.any (\f -> mimeIsImage f.mimetype || mimeIsVideo f.mimetype) (SlackMessage.getFiles m)
                    || List.any slackAttachmentHasImage (SlackMessage.getAttachments m)


mimeIsImage : String -> Bool
mimeIsImage mime =
    String.startsWith "image/" mime


mimeIsVideo : String -> Bool
mimeIsVideo mime =
    String.startsWith "video/" mime


slackAttachmentHasImage : SlackMessage.Attachment -> Bool
slackAttachmentHasImage a =
    case a.imageUrl of
        Just _ ->
            True

        Nothing ->
            False
