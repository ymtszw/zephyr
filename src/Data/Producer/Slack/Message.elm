module Data.Producer.Slack.Message exposing (Message, decoder, decoderForApiResponse, encode, parseOptions)

import AssocList exposing (Dict)
import Color exposing (Color)
import Data.Producer.Slack.Bot as Bot exposing (Bot)
import Data.Producer.Slack.Convo as Convo exposing (Convo)
import Data.Producer.Slack.Message.AngleCmd as AngleCmd
import Data.Producer.Slack.Ts as Ts exposing (Ts)
import Data.Producer.Slack.User as User exposing (User)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Markdown.Inline exposing (Inline(..))
import TextParser
import Url exposing (Url)
import View.Atoms.Theme exposing (aubergineTheme)


{-| A Message object.

There are many properties and functions. Some of them may not be documented well.
<https://api.slack.com/events/message>

`text` is a main contents with marked-up text.
<https://api.slack.com/docs/message-formatting>
Markup includes:

  - URLs: `<https://example.com|Optional Title>`
  - User mention: `<@USERID>`
  - Channel link: `<#CHANNELID|channel name>`
  - Special commands: `<!everyone>`, `<!here>`, `<!channel>`

Better use `leakyList` on decoding.

TODO Support threading nicely

-}
type Message
    = Message MessageRecord


type alias MessageRecord =
    { ts : Ts
    , text : String -- Most likely exists even if empty, but there MIGHT be exceptions.
    , author : Author -- Zephyr local; resolved from `user` or `bot_id`.
    , username : Maybe String -- Manually selected username for a particular Message. Should supercede names in User or Bot
    , files : List SFile
    , attachments : List Attachment
    , conversation : Convo.Id -- Zephyr local; for Filter matching
    }


{-| Bots MAY have an identity as User, but not necessarily.

So if a Message has `bot_id` field, choose BAuthorId.
Bot info must be lazily acquired from `bots.info` API, which does not have corresponding `.list` API.
<https://api.slack.com/methods/bots.info>

Otherwise use UserId from `user` field with UAuthorId.

Since Slack API does not join author info into messages, we must populate them by ourselves.
User/Bot info may be unavailable at first so we must come back and fill.

-}
type Author
    = UserAuthor User
    | UserAuthorId User.Id
    | BotAuthor Bot
    | BotAuthorId Bot.Id


{-| File object. Differentiating from File.File by prefix "S".

<https://api.slack.com/types/file>

Parallel to Discord's Attachment.

TODO also add `url_download_`?

-}
type alias SFile =
    { name : String
    , mimetype : String
    , mode : Mode
    , url_ : Url -- From `url_private` or `permalink`, whichever suitable depending on `mode`. Suffix `_` to differentiate from deprecated `url` field
    , thumb64 : Maybe Url -- For images; smallest and most reliably existing SQUARE thumbnail, with at most 64x64 size
    , thumb360 : Maybe ( Url, Int, Int ) -- For images; larger thumbnail with accurate aspect-ratio. At most 360 width OR height. Comes with width and height
    , preview : Maybe String -- For text contents such as Snippet/Post
    }


{-| File modes. New modes may arrive later.

  - Hosted/External indicate files' location.
  - Snippet is online-editable snippets.
  - Post is online-editable rich text document.

-}
type Mode
    = Hosted
    | External
    | Snippet
    | Post


{-| Attachment object. There are many fields and they are not so nicely documented.

<https://api.slack.com/docs/message-attachments>

This is parallel to Discord's Embeds. Discord's Attachment corresponds to Slack's SFile.

New functions added regularly. We will gradually catch up on them.

Better use `leakyList` on decoding.

-}
type alias Attachment =
    { pretext : Maybe String -- Optional leading text before attachment block
    , color : Maybe Color -- Gutter color of attachment block
    , author : Maybe AttachmentAuthor
    , title : Maybe AttachmentTitle
    , text : String -- Can be empty, and can be marked-up
    , imageUrl : Maybe Url -- Optional image. It is a (possibly external) permalink and not resized/proxied by Slack
    , thumbUrl : Maybe Url -- Optional icon-like thumbnails. Preferred size is 75x75
    , fallback : String -- Plain-text fallback contents without any markup
    }


{-| Aggregated object from `author_name`, `author_link` and `author_icon`.

May use `service_name` and `service_icon` instead, if `author_name` is absent.

-}
type alias AttachmentAuthor =
    { name : String
    , link : Maybe Url
    , icon : Maybe Url
    }


{-| Aggregated object from `title` and `title_link`.
-}
type alias AttachmentTitle =
    { name : String
    , link : Maybe Url
    }



-- Codec


encode : Message -> E.Value
encode (Message m) =
    E.object
        [ ( "ts", Ts.encode m.ts )
        , ( "text", E.string m.text )
        , ( "author", encodeAuthor m.author )
        , ( "username", E.maybe E.string m.username )
        , ( "files", E.list encodeSFile m.files )
        , ( "attachments", E.list encodeAttachment m.attachments )
        , ( "conversation", Id.encode E.string m.conversation )
        ]


encodeAuthor : Author -> E.Value
encodeAuthor aId =
    case aId of
        UserAuthor user ->
            E.tagged "UserAuthor" (User.encode user)

        UserAuthorId userId ->
            E.tagged "UserAuthorId" (Id.encode E.string userId)

        BotAuthor bot ->
            E.tagged "BotAuthor" (Bot.encode bot)

        BotAuthorId botId ->
            E.tagged "BotAuthorId" (Id.encode E.string botId)


encodeSFile : SFile -> E.Value
encodeSFile sf =
    let
        baseProps =
            [ ( "name", E.string sf.name )
            , ( "mimetype", E.string sf.mimetype )
            , ( "mode", encodeMode sf.mode )
            , ( "url_", E.url sf.url_ )
            , ( "thumb_64", E.maybe E.url sf.thumb64 )
            , ( "preview", E.maybe E.string sf.preview )
            ]
    in
    E.object (baseProps ++ encodeThumb360 sf.thumb360)


encodeMode : Mode -> E.Value
encodeMode mode =
    case mode of
        Hosted ->
            E.tag "Hosted"

        External ->
            E.tag "External"

        Snippet ->
            E.tag "Snippet"

        Post ->
            E.tag "Post"


encodeThumb360 : Maybe ( Url, Int, Int ) -> List ( String, E.Value )
encodeThumb360 thumb360 =
    case thumb360 of
        Just ( url, width, height ) ->
            [ ( "thumb_360", E.url url )
            , ( "thumb_360_w", E.int width )
            , ( "thumb_360_h", E.int height )
            ]

        Nothing ->
            []


encodeAttachment : Attachment -> E.Value
encodeAttachment a =
    E.object
        [ ( "pretext", E.maybe E.string a.pretext )
        , ( "color", E.maybe Color.encode a.color )
        , ( "author", E.maybe encodeAttachmentAuthor a.author )
        , ( "title", E.maybe encodeAttachmentTitle a.title )
        , ( "text", E.string a.text )
        , ( "image_url", E.maybe E.url a.imageUrl )
        , ( "thumb_url", E.maybe E.url a.thumbUrl )
        , ( "fallback", E.string a.fallback )
        ]


encodeAttachmentAuthor : AttachmentAuthor -> E.Value
encodeAttachmentAuthor author =
    E.object
        [ ( "name", E.string author.name )
        , ( "link", E.maybe E.url author.link )
        , ( "icon", E.maybe E.url author.icon )
        ]


encodeAttachmentTitle : AttachmentTitle -> E.Value
encodeAttachmentTitle title =
    E.object [ ( "name", E.string title.name ), ( "link", E.maybe E.url title.link ) ]


decoder : Decoder Message
decoder =
    D.map Message <|
        D.map7 MessageRecord
            (D.field "ts" Ts.decoder)
            (D.field "text" D.string)
            (D.field "author" authorDecoder)
            (D.maybeField "username" D.string)
            (D.optionField "files" (D.list sFileDecoder) [])
            (D.optionField "attachments" (D.list attachmentDecoder) [])
            (D.field "conversation" Convo.idDecoder)


authorDecoder : Decoder Author
authorDecoder =
    D.oneOf
        [ D.tagged "UserAuthor" UserAuthor User.decoder
        , D.tagged "UserAuthorId" UserAuthorId User.idDecoder
        , D.tagged "BotAuthor" BotAuthor Bot.decoder
        , D.tagged "BotAuthorId" BotAuthorId Bot.idDecoder
        ]


sFileDecoder : Decoder SFile
sFileDecoder =
    D.do (D.field "mode" modeDecoder) <|
        \mode ->
            D.map7 SFile
                (D.field "name" D.string)
                (D.field "mimetype" D.string)
                (D.succeed mode)
                (sFileUrlDecoder mode)
                (D.maybeField "thumb_64" D.url)
                (D.maybe thumb360Decoder)
                (D.maybeField "preview" D.string)


modeDecoder : Decoder Mode
modeDecoder =
    D.oneOf
        [ D.tag "Hosted" Hosted
        , D.tag "External" External
        , D.tag "Snippet" Snippet
        , D.tag "Post" Post

        -- From Slack API
        , D.do D.string <|
            \str ->
                D.succeed <|
                    case str of
                        "hosted" ->
                            Hosted

                        "external" ->
                            External

                        "snippet" ->
                            Snippet

                        "post" ->
                            Post

                        "space" ->
                            -- Don't know why but Posts can have mode: "space"
                            Post

                        _ ->
                            -- Treated as a fallback
                            External
        ]


sFileUrlDecoder : Mode -> Decoder Url
sFileUrlDecoder mode =
    D.oneOf
        [ D.field "url_" D.url

        -- From Slack API;
        -- `permalink` opens default viewer/redirecter in Slack, whereas `url_private` opens actual file entity
        -- In case of Snippet/Post, file entites are plain texts or JSON files, so opening with default viewer would make more sense
        , case mode of
            Hosted ->
                D.field "url_private" D.url

            External ->
                D.field "url_private" D.url

            Snippet ->
                D.field "permalink" D.url

            Post ->
                D.field "permalink" D.url
        ]


thumb360Decoder : Decoder ( Url, Int, Int )
thumb360Decoder =
    D.do (D.field "thumb_360" D.url) <|
        \u ->
            D.do (D.field "thumb_360_w" D.int) <|
                \w ->
                    D.do (D.field "thumb_360_h" D.int) <|
                        \h ->
                            D.succeed ( u, w, h )


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    D.map8 Attachment
        (D.maybeField "pretext" D.string)
        (D.maybeField "color" colorDecoder)
        (D.maybe attachmentAuthorDecoder)
        (D.maybe attachmentTitleDecoder)
        -- `text` can be absent!!
        (D.optionField "text" D.string "")
        (D.maybeField "image_url" D.url)
        (D.maybeField "thumb_url" D.url)
        (D.field "fallback" D.string)


colorDecoder : Decoder Color
colorDecoder =
    D.oneOf
        [ Color.decoder
        , -- From Slack API
          D.do D.string <|
            \str ->
                case str of
                    "good" ->
                        D.succeed aubergineTheme.succ

                    "warning" ->
                        D.succeed aubergineTheme.warn

                    "danger" ->
                        D.succeed aubergineTheme.err

                    _ ->
                        D.fail ("Unknown color string: " ++ str)
        ]


attachmentAuthorDecoder : Decoder AttachmentAuthor
attachmentAuthorDecoder =
    D.oneOf
        [ D.field "author" <|
            D.map3 AttachmentAuthor
                (D.field "name" D.string)
                (D.field "link" (D.maybe D.url))
                (D.field "icon" (D.maybe D.url))

        -- From Slack API; in official app, author and service are both presented but it felt kind of redundant
        , D.map3 AttachmentAuthor
            (D.oneOf [ D.field "author_name" D.string, D.field "service_name" D.string ])
            -- Subtle, but an important difference; in oneOf, earlier decoders must "fail" in order to proceed to other decoders.
            -- D.maybeField always succeeds (with Nothing, at least) so it can shadow remaining decoders when used earlier.
            (D.oneOf [ D.field "author_link" D.url |> D.map Just, D.maybeField "service_url" D.url ])
            (D.oneOf [ D.field "author_icon" D.url |> D.map Just, D.maybeField "service_icon" D.url ])
        ]


attachmentTitleDecoder : Decoder AttachmentTitle
attachmentTitleDecoder =
    D.oneOf
        [ D.field "title" <|
            D.map2 AttachmentTitle (D.field "name" D.string) (D.maybeField "link" D.url)

        -- From Slack API
        , D.map2 AttachmentTitle (D.field "title" D.string) (D.maybeField "title_link" D.url)
        ]


decoderForApiResponse :
    Dict User.Id User
    -> Dict Bot.Id Bot
    -> Dict Convo.Id Convo
    -> Convo.Id
    -> Decoder Message
decoderForApiResponse users bots convos convoId =
    let
        apiAuthorDecoder =
            D.oneOf
                [ D.do (D.field "bot_id" Bot.idDecoder) <|
                    \botId ->
                        case AssocList.get botId bots of
                            Just bot ->
                                D.succeed (BotAuthor bot)

                            Nothing ->
                                D.succeed (BotAuthorId botId)
                , D.do (D.field "user" User.idDecoder) <|
                    \userId ->
                        case AssocList.get userId users of
                            Just user ->
                                D.succeed (UserAuthor user)

                            Nothing ->
                                D.succeed (UserAuthorId userId)
                ]
    in
    D.map Message <|
        D.map7 MessageRecord
            (D.field "ts" Ts.decoder)
            (D.field "text" (D.map (AngleCmd.resolve convos users) D.string))
            apiAuthorDecoder
            (D.maybeField "username" D.string)
            (D.optionField "files" (D.leakyList sFileDecoder) [])
            (D.optionField "attachments" (D.leakyList (apiAttachmentDecoder convos users)) [])
            (D.succeed convoId)


apiAttachmentDecoder : Dict Convo.Id Convo -> Dict User.Id User -> Decoder Attachment
apiAttachmentDecoder convs users =
    D.map8 Attachment
        (D.maybeField "pretext" (D.map (AngleCmd.resolve convs users) D.string))
        (D.maybeField "color" colorDecoder)
        (D.maybe attachmentAuthorDecoder)
        (D.maybe attachmentTitleDecoder)
        -- `text` can be absent!!
        (D.optionField "text" (D.map (AngleCmd.resolve convs users) D.string) "")
        (D.maybeField "image_url" D.url)
        (D.maybeField "thumb_url" D.url)
        (D.field "fallback" (D.map (AngleCmd.resolve convs users) D.string))



-- Markdown Parse Option


parseOptions : TextParser.ParseOptions
parseOptions =
    { markdown = True
    , autoLink = False
    , unescapeTags = True
    , preFormat = Nothing
    , customInlineFormat = Just alterEmphasis
    }


alterEmphasis : Inline () -> Inline ()
alterEmphasis inline =
    case inline of
        Emphasis level inlines ->
            -- In Slack `*` is treated as level 2 and `_` as level 1, but this does not conform with CommonMark spec.
            -- Since `*` is more commonly used, we force level 2 in order to keep visuals in-line with official Slack app.
            if level < 2 then
                Emphasis 2 inlines

            else
                Emphasis level inlines

        _ ->
            inline
