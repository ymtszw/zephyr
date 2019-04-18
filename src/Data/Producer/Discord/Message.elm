module Data.Producer.Discord.Message exposing
    ( Message, Id, Author, Attachment, Embed, encode, decoder
    , getId, getChannelId, getAuthor, getTimestamp, getContent, getEmbeds, getAttachments
    , parseOptions
    )

{-| Message object.

@docs Message, Id, Author, Attachment, Embed, encode, decoder
@docs getId, getChannelId, getAuthor, getTimestamp, getContent, getEmbeds, getAttachments
@docs parseOptions

-}

import Color exposing (Color)
import Data.Producer.Discord.Channel as Channel
import Data.Producer.Discord.User as User exposing (User)
import Hex
import Id
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import TextParser
import Time exposing (Posix)
import Url exposing (Url)


{-| Discord Message Object.

Only interested in "type: 0" (DEFAULT) messages.
`guild_id` is somewhat missing, though it can be derived from Channel anyway.

<https://discordapp.com/developers/docs/resources/channel#message-object>

TODO use reactions, with introducing delayed update mechanism

-}
type Message
    = Message MessageRecord


type alias MessageRecord =
    { id : Id
    , channelId : Channel.Id
    , author : Author
    , timestamp : Posix
    , content : String
    , embeds : List Embed
    , attachments : List Attachment
    }


type alias Id =
    Id.Id String Message


type Author
    = UserAuthor User
    | WebhookAuthor User


{-| Embed object.

TODO There are more fields to be added. Some bots rely on them.
<https://discordapp.com/developers/docs/resources/channel#embed-object>

-}
type alias Embed =
    { title : Maybe String
    , description : Maybe String
    , url : Maybe Url
    , color : Maybe Color
    , image : Maybe EmbedImage
    , thumbnail : Maybe EmbedImage -- Embed thumbnail and image are identical in structure
    , video : Maybe EmbedVideo
    , author : Maybe EmbedAuthor
    }


type alias EmbedImage =
    -- XXX Embed can use attachment via attachment ID as url. Might support later.
    { url : Url
    , proxyUrl : Maybe Url
    , height : Maybe Int
    , width : Maybe Int
    }


type alias EmbedVideo =
    { url : Url
    , height : Maybe Int
    , width : Maybe Int
    }


type alias EmbedAuthor =
    { name : String
    , url : Maybe Url
    , iconUrl : Maybe Url
    , proxyIconUrl : Maybe Url
    }


type alias Attachment =
    { filename : String
    , url : Url
    , proxyUrl : Url
    , height : Maybe Int
    , width : Maybe Int
    }



-- Codec


encode : Message -> E.Value
encode (Message message) =
    E.object
        [ ( "id", Id.encode E.string message.id )
        , ( "channel_id", Id.encode E.string message.channelId )
        , ( "type", E.int 0 )
        , ( "author", encodeAuthor message.author )
        , ( "timestamp", Iso8601.encode message.timestamp )
        , ( "content", E.string message.content )
        , ( "embeds", E.list encodeEmbed message.embeds )
        , ( "attachments", E.list encodeAttachment message.attachments )
        ]


encodeAuthor : Author -> E.Value
encodeAuthor author =
    case author of
        UserAuthor user ->
            E.tagged "UserAuthor" (User.encode user)

        WebhookAuthor user ->
            E.tagged "WebhookAuthor" (User.encode user)


encodeEmbed : Embed -> E.Value
encodeEmbed embed =
    E.object
        [ ( "title", E.maybe E.string embed.title )
        , ( "description", E.maybe E.string embed.description )
        , ( "url", E.maybe E.url embed.url )
        , ( "color", E.maybe Color.encode embed.color )
        , ( "image", E.maybe encodeEmbedImage embed.image )
        , ( "thumbnail", E.maybe encodeEmbedImage embed.thumbnail )
        , ( "video", E.maybe encodeEmbedVideo embed.video )
        , ( "author", E.maybe encodeEmbedAuthor embed.author )
        ]


encodeEmbedImage : EmbedImage -> E.Value
encodeEmbedImage eImage =
    E.object
        [ ( "url", E.url eImage.url )
        , ( "proxy_url", E.maybe E.url eImage.proxyUrl )
        , ( "height", E.maybe E.int eImage.height )
        , ( "width", E.maybe E.int eImage.width )
        ]


encodeEmbedVideo : EmbedVideo -> E.Value
encodeEmbedVideo eVideo =
    E.object
        [ ( "url", E.url eVideo.url )
        , ( "height", E.maybe E.int eVideo.height )
        , ( "width", E.maybe E.int eVideo.width )
        ]


encodeEmbedAuthor : EmbedAuthor -> E.Value
encodeEmbedAuthor eAuthor =
    E.object
        [ ( "name", E.string eAuthor.name )
        , ( "url", E.maybe E.url eAuthor.url )
        , ( "icon_url", E.maybe E.url eAuthor.iconUrl )
        , ( "proxy_icon_url", E.maybe E.url eAuthor.proxyIconUrl )
        ]


encodeAttachment : Attachment -> E.Value
encodeAttachment attachment =
    E.object
        [ ( "filename", E.string attachment.filename )
        , ( "url", E.string (Url.toString attachment.url) )
        , ( "proxy_url", E.string (Url.toString attachment.proxyUrl) )
        , ( "height", E.maybe E.int attachment.height )
        , ( "width", E.maybe E.int attachment.width )
        ]


decoder : Decoder Message
decoder =
    -- Only care about DEFAULT message type; otherwise fail, thus you must use this with DecodeExtra.leakyList
    D.when (D.field "type" D.int) ((==) 0) <|
        D.map Message <|
            D.map7 MessageRecord
                (D.field "id" (Id.decoder D.string))
                (D.field "channel_id" (Id.decoder D.string))
                authorDecoder
                (D.field "timestamp" Iso8601.decoder)
                (D.field "content" D.string)
                (D.field "embeds" (D.list embedDecoder))
                (D.field "attachments" (D.list attachmentDecoder))


authorDecoder : Decoder Author
authorDecoder =
    let
        decodeFromDiscordApi =
            D.do (D.maybeField "webhook_id" D.string) <|
                \webhookMaybe ->
                    case webhookMaybe of
                        Just _ ->
                            D.field "author" (D.map WebhookAuthor User.decoder)

                        Nothing ->
                            D.field "author" (D.map UserAuthor User.decoder)
    in
    D.oneOf
        [ decodeFromDiscordApi
        , D.field "author" <| D.tagged "UserAuthor" UserAuthor User.decoder
        , D.field "author" <| D.tagged "WebhookAuthor" WebhookAuthor User.decoder
        ]


embedDecoder : Decoder Embed
embedDecoder =
    D.map8 Embed
        (D.maybeField "title" D.string)
        (D.maybeField "description" D.string)
        (D.maybeField "url" D.url)
        (D.maybeField "color" colorDecoder)
        (D.maybeField "image" embedImageDecoder)
        (D.maybeField "thumbnail" embedImageDecoder)
        (D.maybeField "video" embedVideoDecoder)
        (D.maybeField "author" embedAuthorDecoder)


colorDecoder : Decoder Color
colorDecoder =
    D.oneOf
        [ Color.decoder
        , -- From Discord API
          D.do D.int <|
            \int ->
                case int |> Hex.toString |> String.padLeft 6 '0' |> Color.fromHex of
                    Ok c ->
                        D.succeed c

                    Err _ ->
                        D.fail ("Invalid color code: " ++ String.fromInt int)
        ]


embedImageDecoder : Decoder EmbedImage
embedImageDecoder =
    D.map4 EmbedImage
        (D.field "url" D.url)
        (D.maybeField "proxy_url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)


embedVideoDecoder : Decoder EmbedVideo
embedVideoDecoder =
    D.map3 EmbedVideo
        (D.field "url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)


embedAuthorDecoder : Decoder EmbedAuthor
embedAuthorDecoder =
    D.map4 EmbedAuthor
        (D.field "name" D.string)
        (D.maybeField "url" D.url)
        (D.maybeField "icon_url" D.url)
        (D.maybeField "proxy_icon_url" D.url)


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    D.map5 Attachment
        (D.field "filename" D.string)
        (D.field "url" D.url)
        (D.field "proxy_url" D.url)
        (D.maybeField "height" D.int)
        (D.maybeField "width" D.int)



-- Runtime APis


getId : Message -> Id
getId (Message message) =
    message.id


getChannelId : Message -> Channel.Id
getChannelId (Message message) =
    message.channelId


getAuthor : Message -> Author
getAuthor (Message message) =
    message.author


getTimestamp : Message -> Posix
getTimestamp (Message message) =
    message.timestamp


getContent : Message -> String
getContent (Message message) =
    message.content


getEmbeds : Message -> List Embed
getEmbeds (Message message) =
    message.embeds


getAttachments : Message -> List Attachment
getAttachments (Message message) =
    message.attachments


parseOptions : TextParser.ParseOptions
parseOptions =
    { markdown = True
    , autoLink = True
    , unescapeTags = False
    , preFormat = Nothing
    , customInlineFormat = Nothing
    }
