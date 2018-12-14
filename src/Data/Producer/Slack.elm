module Data.Producer.Slack exposing
    ( Slack(..), SlackUnidentified(..), SlackRegistry, User, Team, TeamIcon
    , Conversation, ConversationType(..), ConversationCache, FAM
    , initRegistry, encodeRegistry, registryDecoder, encodeUser, userDecoder, encodeTeam, teamDecoder
    , encodeConversation, conversationDecoder, encodeConversationCache, conversationCacheDecoder
    , encodeBot, botDecoder, encodeMessage, messageDecoder, encodeFam, famDecoder
    , Msg(..), RpcFailure(..), reload, update
    , getUser, isChannel, compareByMembersipThenName, getConversationIdStr
    , defaultIconUrl, teamUrl, dummyConversationId, getConversationFromCache
    )

{-| Producer for Slack workspaces.

Slack API uses HTTP RPC style. See here for available methods:
<https://api.slack.com/methods>

@docs Slack, SlackUnidentified, SlackRegistry, User, Team, TeamIcon
@docs Conversation, ConversationType, ConversationCache, FAM
@docs initRegistry, encodeRegistry, registryDecoder, encodeUser, userDecoder, encodeTeam, teamDecoder
@docs encodeConversation, conversationDecoder, encodeConversationCache, conversationCacheDecoder
@docs encodeBot, botDecoder, encodeMessage, messageDecoder, encodeFam, famDecoder
@docs Msg, RpcFailure, reload, update
@docs getUser, isChannel, compareByMembersipThenName, getConversationIdStr
@docs defaultIconUrl, teamUrl, dummyConversationId, getConversationFromCache

-}

import Data.ColorTheme exposing (aubergine)
import Data.Filter exposing (FilterAtom)
import Data.Producer as Producer exposing (..)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Dict exposing (Dict)
import Element
import Http
import HttpClient exposing (noAuth)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import ListExtra
import Task exposing (Task)
import Time exposing (Posix)
import Url exposing (Url)
import Worque


{-| A primary state machine of Slack team-identity combination.

Likely we require a legacy token, but could use a user token in the future.
<https://api.slack.com/custom-integrations/legacy-tokens>
First we get user ID and team ID from `auth.test` API,
<https://api.slack.com/methods/auth.test>
then fetch User and Team using `users.info` and `team.info`, populating `NewSession`.

Since Slack APIs mostly return "not joined" data,
we have to maintain User dictionary in order to show their info.
We should occasionally (lazily) update it.

-}
type Slack
    = Identified NewSession
    | Hydrated String POV
    | Rehydrating String POV
    | Revisit POV
    | Expired String POV


type alias NewSession =
    { token : String
    , user : User
    , team : Team
    }


type alias POV =
    { token : String
    , user : User
    , team : Team
    , conversations : Dict ConversationIdStr Conversation
    , users : Dict UserIdStr User
    , bots : Dict BotIdStr Bot -- Lazily populated/updated
    }


{-| A user object.

<https://api.slack.com/types/user>
<https://api.slack.com/methods/users.info>

-}
type alias User =
    { id : UserId
    , teamId : TeamId
    , profile : UserProfile
    }


type UserId
    = UserId UserIdStr


type alias UserIdStr =
    String


type alias UserProfile =
    { displayName : Maybe String -- Can be empty string!! Treat empty string as Nothing
    , realName : String
    , statusText : String
    , statusEmoji : String
    , image32 : Url
    , image48 : Url
    }


{-| A team object.

<https://api.slack.com/methods/team.info>

-}
type alias Team =
    { id : TeamId
    , name : String
    , domain : String
    , icon : TeamIcon
    }


type alias TeamIcon =
    { image34 : Url
    , image44 : Url
    , image68 : Url
    , imageDefault : Bool
    }


type TeamId
    = TeamId TeamIdStr


type alias TeamIdStr =
    String


{-| Channel-like objects. Includes public/private channels and IM/MPIMs.

<https://api.slack.com/types/conversation>
<https://api.slack.com/methods/conversations.list>

For IMs, we need the other member's User object for its name.

For MPIMs, we should consider how `name`s are shown,
since `name` values of MPIMs are auto-generated strings.

Members of private conversations must be retrieved from conversations.members API.
<https://api.slack.com/methods/conversations.members>
(Although conversation doc says there is `members` field, it is retired.
<https://api.slack.com/changelog/2017-10-members-array-truncating>)

We do not use `last_read` returned from APIs directly,
rather we locally record timestamps of actually retrieved messages.

TODO: consider how to support IM/MPIMs, while aligning with Discord DM/GroupDM

-}
type alias Conversation =
    { id : ConversationId
    , name : String -- May start with values from API response, but should cannonicalize on Hydrate/Rehydrate
    , lastRead : Maybe LastRead -- For IM, `last_read` is not supplied from conversation object. Though we do not directly use them anyway.

    -- Zephyr local
    , type_ : ConversationType
    , fetchStatus : FetchStatus
    }


type ConversationId
    = ConversationId ConversationIdStr


type alias ConversationIdStr =
    String


type LastRead
    = LastRead Ts


type ConversationType
    = PublicChannel IsMember
    | PrivateChannel
    | IM -- Instant Messages, presumably
    | MPIM -- Multi-person IM


type alias IsMember =
    Bool


{-| Rarely updated prts of Conversation.

Adding Team info since ConversationCaches can be mixed up with ones from other Teams in FAM.

-}
type alias ConversationCache =
    { id : ConversationId
    , name : String
    , type_ : ConversationType
    , team : Team -- Unjoined on save
    }


{-| Bot entity in the workspace.

A Bot is somewhat User-like, but different in many ways.
<https://api.slack.com/methods/bots.info>

We cannot enumerate available bots ahead of time by API (`bots.list` does not exist),
so we must lazily acquire/update Bots' info when we see `bot_id` in messages, and store them in local dictionary.

-}
type alias Bot =
    { id : BotId
    , name : String
    , icons : BotIcons -- Yeah, plural
    }


type alias BotIcons =
    { image36 : Url
    , image48 : Url
    , image72 : Url
    }


type BotId
    = BotId BotIdStr


type alias BotIdStr =
    String


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
type alias Message =
    { ts : Ts
    , text : String -- Most likely exists even if empty, but there MIGHT be exceptions.
    , authorId : AuthorId -- Zephyr local; from `user` or `bot_id`
    , username : Maybe String -- Manually selected username for a particular Message. Should supercede names in User or Bot
    , files : List SFile
    , attachements : List Attachment
    }


{-| Slack messages' ID AND timestamps.

This is unique per channel, sorted, and can be translated as Posix timestamps.

For convenience, they are parsed as Time.Posix at decoding and results are attached in the type.

-}
type Ts
    = Ts String Posix


{-| Bots MAY have an identity as User, but not necessarily.

So if a Message has `bot_id` field, choose BAuthorId.
Bot info must be lazily acquired from `bots.info` API, which does not have corresponding `.list` API.
<https://api.slack.com/methods/bots.info>

Otherwise use UserId from `user` field with UAuthorId.

-}
type AuthorId
    = UAuthorId UserId
    | BAuthorId BotId


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
    , color : Maybe Element.Color -- Gutter color of attachment block
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


{-| Runtime registry of multiple Slack state machines.
-}
type alias SlackRegistry =
    { dict : Dict TeamIdStr Slack
    , unidentified : SlackUnidentified
    }


{-| Not yet identified token states.
-}
type SlackUnidentified
    = TokenWritable String
    | TokenIdentifying String


initRegistry : SlackRegistry
initRegistry =
    { dict = Dict.empty, unidentified = initUnidentified }


initUnidentified : SlackUnidentified
initUnidentified =
    TokenWritable ""


type alias FAM =
    { default : FilterAtom
    , conversations : List ConversationCache -- List instead of Dict, should be sorted already
    }



-- Encode


encodeRegistry : SlackRegistry -> E.Value
encodeRegistry sr =
    E.object
        [ ( "dict", E.dict identity encodeSlack sr.dict )
        , ( "unidentified", encodeUnidentified sr.unidentified )
        ]


encodeUnidentified : SlackUnidentified -> E.Value
encodeUnidentified su =
    case su of
        TokenWritable t ->
            E.tagged "TokenWritable" (E.string t)

        TokenIdentifying t ->
            E.tagged "TokenIdentifying" (E.string t)


encodeSlack : Slack -> E.Value
encodeSlack slack =
    case slack of
        Identified session ->
            E.tagged "Identified" (encodeSession session)

        Hydrated _ pov ->
            E.tagged "Revisit" (encodePov pov)

        Rehydrating _ pov ->
            E.tagged "Revisit" (encodePov pov)

        Revisit pov ->
            E.tagged "Revisit" (encodePov pov)

        Expired _ pov ->
            E.tagged "Revisit" (encodePov pov)


encodeSession : NewSession -> E.Value
encodeSession session =
    E.object
        [ ( "token", E.string session.token )
        , ( "user", encodeUser session.user )
        , ( "team", encodeTeam session.team )
        ]


encodePov : POV -> E.Value
encodePov pov =
    E.object
        [ ( "token", E.string pov.token )
        , ( "user", encodeUser pov.user )
        , ( "team", encodeTeam pov.team )
        , ( "conversations", E.dict identity encodeConversation pov.conversations )
        , ( "users", E.dict identity encodeUser pov.users )
        , ( "bots", E.dict identity encodeBot pov.bots )
        ]


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "id", encodeUserId user.id )
        , ( "team_id", encodeTeamId user.teamId )
        , Tuple.pair "profile" <|
            E.object
                [ ( "display_name", E.string (Maybe.withDefault "" user.profile.displayName) ) -- In-line with Slack API
                , ( "real_name", E.string user.profile.realName )
                , ( "status_text", E.string user.profile.statusText )
                , ( "status_emoji", E.string user.profile.statusEmoji )
                , ( "image_32", E.url user.profile.image32 )
                , ( "image_48", E.url user.profile.image48 )
                ]
        ]


encodeUserId : UserId -> E.Value
encodeUserId (UserId id) =
    E.tagged "UserId" (E.string id)


encodeBot : Bot -> E.Value
encodeBot bot =
    E.object
        [ ( "id", encodeBotId bot.id )
        , ( "name", E.string bot.name )
        , Tuple.pair "icons" <|
            E.object
                [ ( "image_36", E.url bot.icons.image36 )
                , ( "image_48", E.url bot.icons.image48 )
                , ( "image_72", E.url bot.icons.image72 )
                ]
        ]


encodeBotId : BotId -> E.Value
encodeBotId (BotId bId) =
    E.tagged "BotId" (E.string bId)


encodeTeamId : TeamId -> E.Value
encodeTeamId (TeamId id) =
    E.tagged "TeamId" (E.string id)


encodeTeam : Team -> E.Value
encodeTeam team =
    E.object
        [ ( "id", encodeTeamId team.id )
        , ( "name", E.string team.name )
        , ( "domain", E.string team.domain )
        , Tuple.pair "icon" <|
            E.object
                [ ( "image_34", E.url team.icon.image34 )
                , ( "image_44", E.url team.icon.image44 )
                , ( "image_68", E.url team.icon.image68 )
                , ( "image_default", E.bool team.icon.imageDefault )
                ]
        ]


encodeConversation : Conversation -> E.Value
encodeConversation conv =
    E.object
        [ ( "id", encodeConversationId conv.id )
        , ( "name", E.string conv.name )
        , ( "last_read", E.maybe encodeLastRead conv.lastRead )
        , ( "type_", encodeConversationType conv.type_ )
        , ( "fetchStatus", FetchStatus.encode conv.fetchStatus )
        ]


encodeConversationId : ConversationId -> E.Value
encodeConversationId (ConversationId convId) =
    E.tagged "ConversationId" (E.string convId)


encodeLastRead : LastRead -> E.Value
encodeLastRead (LastRead lastRead) =
    E.tagged "LastRead" (encodeTs lastRead)


encodeConversationCache : ConversationCache -> E.Value
encodeConversationCache cache =
    E.object
        [ ( "id", encodeConversationId cache.id )
        , ( "name", E.string cache.name )
        , ( "type_", encodeConversationType cache.type_ )
        , ( "team_id", encodeTeamId cache.team.id )
        ]


encodeConversationType : ConversationType -> E.Value
encodeConversationType type_ =
    case type_ of
        PublicChannel isMember ->
            E.tagged "PublicChannel" (E.bool isMember)

        PrivateChannel ->
            E.tag "PrivateChannel"

        IM ->
            E.tag "IM"

        MPIM ->
            E.tag "MPIM"


encodeMessage : Message -> E.Value
encodeMessage m =
    E.object
        [ ( "ts", encodeTs m.ts )
        , ( "text", E.string m.text )
        , ( "authorId", encodeAuthorId m.authorId )
        , ( "username", E.maybe E.string m.username )
        , ( "files", E.list encodeSFile m.files )
        , ( "attachements", E.list encodeAttachment m.attachements )
        ]


encodeTs : Ts -> E.Value
encodeTs (Ts ts posix) =
    E.tagged2 "Ts" (E.string ts) (E.int (Time.posixToMillis posix))


encodeAuthorId : AuthorId -> E.Value
encodeAuthorId aId =
    case aId of
        UAuthorId userId ->
            E.tagged "UAuthorId" (encodeUserId userId)

        BAuthorId botId ->
            E.tagged "BAuthorId" (encodeBotId botId)


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
        , ( "color", E.maybe E.color a.color )
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


encodeFam : FAM -> E.Value
encodeFam fam =
    let
        ( encodedConvs, teams ) =
            unjoinTeamsAndEncodeConvs fam.conversations
    in
    E.object
        [ ( "default", Data.Filter.encodeFilterAtom fam.default )
        , ( "conversations", E.list identity encodedConvs )
        , ( "teams", E.dict identity encodeTeam teams )
        ]


unjoinTeamsAndEncodeConvs : List ConversationCache -> ( List E.Value, Dict TeamIdStr Team )
unjoinTeamsAndEncodeConvs conversations =
    let
        reducer c ( accList, accTeams ) =
            let
                (TeamId teamIdStr) =
                    c.team.id
            in
            ( encodeConversationCache c :: accList, Dict.insert teamIdStr c.team accTeams )
    in
    -- Conserve the order of conversations; already sorted
    List.foldr reducer ( [], Dict.empty ) conversations



-- Decoder


registryDecoder : Decoder SlackRegistry
registryDecoder =
    D.oneOf
        [ D.map2 SlackRegistry
            (D.field "dict" (D.dict slackDecoder))
            (D.field "unidentified" unidentifiedDecoder)
        , D.succeed initRegistry -- fallback
        ]


unidentifiedDecoder : Decoder SlackUnidentified
unidentifiedDecoder =
    D.oneOf
        [ D.tagged "TokenWritable" TokenWritable D.string
        , D.tagged "TokenIdentifying" TokenIdentifying D.string
        ]


slackDecoder : Decoder Slack
slackDecoder =
    D.oneOf
        [ D.tagged "Identified" Identified sessionDecoder
        , D.tagged "Revisit" Revisit povDecoder

        -- Old formats
        , D.tagged2 "Hydrated" (\_ pov -> Revisit pov) D.string povDecoder
        ]


sessionDecoder : Decoder NewSession
sessionDecoder =
    D.map3 NewSession
        (D.field "token" D.string)
        (D.field "user" userDecoder)
        (D.field "team" teamDecoder)


povDecoder : Decoder POV
povDecoder =
    D.do (D.field "users" (D.dict userDecoder)) <|
        \users ->
            D.map6 POV
                (D.field "token" D.string)
                (D.field "user" userDecoder)
                (D.field "team" teamDecoder)
                (D.field "conversations" (D.dict (conversationDecoder users)))
                (D.succeed users)
                -- Migration; use D.field when ready
                (D.optionField "bots" (D.dict botDecoder) Dict.empty)


userDecoder : Decoder User
userDecoder =
    let
        profileDecoder =
            D.map6 UserProfile
                (D.field "display_name" nonEmptyStringDecoder)
                (D.field "real_name" D.string)
                (D.field "status_text" D.string)
                (D.field "status_emoji" D.string)
                (D.field "image_32" D.url)
                (D.field "image_48" D.url)

        nonEmptyStringDecoder =
            D.do D.string <|
                \s ->
                    if String.isEmpty s then
                        D.succeed Nothing

                    else
                        D.succeed (Just s)
    in
    D.map3 User
        (D.field "id" userIdDecoder)
        (D.field "team_id" teamIdDecoder)
        (D.field "profile" profileDecoder)


userIdDecoder : Decoder UserId
userIdDecoder =
    D.oneOf [ D.tagged "UserId" UserId D.string, D.map UserId D.string ]


botDecoder : Decoder Bot
botDecoder =
    let
        iconsDecoder =
            D.map3 BotIcons
                (D.field "image_36" D.url)
                (D.field "image_48" D.url)
                (D.field "image_72" D.url)
    in
    D.map3 Bot
        (D.field "id" botIdDecoder)
        (D.field "name" D.string)
        (D.field "icons" iconsDecoder)


botIdDecoder : Decoder BotId
botIdDecoder =
    D.oneOf [ D.tagged "BotId" BotId D.string, D.map BotId D.string ]


teamIdDecoder : Decoder TeamId
teamIdDecoder =
    D.oneOf [ D.tagged "TeamId" TeamId D.string, D.map TeamId D.string ]


teamDecoder : Decoder Team
teamDecoder =
    let
        iconDecoder =
            -- image_default is actually absent when false
            D.map4 TeamIcon
                (D.field "image_34" D.url)
                (D.field "image_44" D.url)
                (D.field "image_68" D.url)
                (D.optionField "image_default" D.bool False)
    in
    D.map4 Team
        (D.field "id" teamIdDecoder)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "icon" iconDecoder)


conversationDecoder : Dict UserIdStr User -> Decoder Conversation
conversationDecoder users =
    -- XXX We are including archived channels and IM with deleted users.
    let
        conversationFromSlackApiDecoder =
            D.do conversationTypeFromSlackApi <|
                \type_ ->
                    D.do (resolveName type_) <|
                        \name ->
                            D.do (D.field "id" conversationIdDecoder) <|
                                \id ->
                                    D.succeed (Conversation id name Nothing type_ Available)

        conversationTypeFromSlackApi =
            D.oneOf
                [ D.when (D.field "is_mpim" D.bool) identity (D.succeed MPIM)
                , D.when (D.field "is_im" D.bool) identity (D.succeed IM)
                , D.when (D.field "is_group" D.bool) identity (D.succeed PrivateChannel)
                , -- The doc says it is a public channel when is_channel: true but it is possible to be paired with is_private: true
                  D.when (D.map2 (&&) (D.field "is_channel" D.bool) (D.field "is_private" D.bool)) identity (D.succeed PrivateChannel)
                , D.when (D.map2 Tuple.pair (D.field "is_channel" D.bool) (D.field "is_private" D.bool)) ((==) ( True, False )) <|
                    D.field "is_member" (D.map PublicChannel D.bool)
                ]

        resolveName type_ =
            case type_ of
                IM ->
                    D.field "user" userIdDecoder |> D.andThen (resolveUserNameDecoder users)

                MPIM ->
                    -- TODO cannonicalize name; need to request to `conversations.members`?
                    D.field "name" D.string

                _ ->
                    D.field "name" D.string
    in
    D.oneOf
        [ -- From IndexedDB
          D.map5 Conversation
            (D.field "id" conversationIdDecoder)
            (D.field "name" D.string)
            (D.maybeField "last_read" lastReadDecoder)
            (D.field "type_" conversationTypeDecoder)
            (D.optionField "fetchStatus" FetchStatus.decoder Available)

        -- From Slack API
        , conversationFromSlackApiDecoder

        -- Old format where Conversation was a custom type
        , oldConversationDecoder users
        ]


conversationIdDecoder : Decoder ConversationId
conversationIdDecoder =
    D.oneOf [ D.tagged "ConversationId" ConversationId D.string, D.map ConversationId D.string ]


lastReadDecoder : Decoder LastRead
lastReadDecoder =
    -- As in Discord's lastMessageId, we deliberately ignore "last_read" from Slack API.
    D.tagged "LastRead" LastRead tsDecoder


conversationTypeDecoder : Decoder ConversationType
conversationTypeDecoder =
    D.oneOf
        [ D.tagged "PublicChannel" PublicChannel D.bool
        , D.tag "PrivateChannel" PrivateChannel
        , D.tag "IM" IM
        , D.tag "MPIM" MPIM

        -- Old formats where ConversationType was only used in caches
        , D.tag "PublicChannelCache" (PublicChannel True) -- Making up True for new type, but IsMember must be properly rehydrated
        , D.tag "PrivateChannelCache" PrivateChannel
        , D.tag "IMCache" IM
        , D.tag "MPIMCache" MPIM
        ]


resolveUserNameDecoder : Dict UserIdStr User -> UserId -> Decoder String
resolveUserNameDecoder users (UserId userIdStr) =
    Dict.get userIdStr users
        |> Maybe.map (\u -> Maybe.withDefault u.profile.realName u.profile.displayName)
        |> Maybe.withDefault userIdStr
        |> D.succeed


oldConversationDecoder : Dict UserIdStr User -> Decoder Conversation
oldConversationDecoder users =
    let
        oldPubDecoder =
            D.map5 Conversation
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.maybeField "last_read" lastReadDecoder)
                (D.map PublicChannel (D.optionField "is_member" D.bool False))
                (D.optionField "fetchStatus" FetchStatus.decoder Available)

        oldPrivDecoder =
            D.map5 Conversation
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.maybeField "last_read" lastReadDecoder)
                (D.succeed PrivateChannel)
                (D.optionField "fetchStatus" FetchStatus.decoder Available)

        oldImDecoder =
            D.do (D.field "user" userIdDecoder) <|
                \userId ->
                    D.map5 Conversation
                        (D.field "id" conversationIdDecoder)
                        (resolveUserNameDecoder users userId)
                        (D.maybeField "last_read" lastReadDecoder)
                        (D.succeed IM)
                        (D.optionField "fetchStatus" FetchStatus.decoder Available)

        oldMpimDecoder =
            D.map5 Conversation
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.maybeField "last_read" lastReadDecoder)
                (D.succeed MPIM)
                (D.optionField "fetchStatus" FetchStatus.decoder Available)
    in
    D.oneOf
        [ D.tagged "PublicChannel" identity oldPubDecoder
        , D.tagged "PrivateChannel" identity oldPrivDecoder
        , D.tagged "IM" identity oldImDecoder
        , D.tagged "MPIM" identity oldMpimDecoder
        ]


conversationCacheDecoder : Dict TeamIdStr Team -> Decoder ConversationCache
conversationCacheDecoder teams =
    D.do (D.field "team_id" teamIdDecoder) <|
        \(TeamId teamIdStr) ->
            case Dict.get teamIdStr teams of
                Just team ->
                    D.map4 ConversationCache
                        (D.field "id" conversationIdDecoder)
                        (D.field "name" D.string)
                        (D.field "type_" conversationTypeDecoder)
                        (D.succeed team)

                Nothing ->
                    -- Should not happen
                    D.fail ("Team [" ++ teamIdStr ++ "] is not cached!")


messageDecoder : Decoder Message
messageDecoder =
    D.map6 Message
        (D.field "ts" tsDecoder)
        (D.field "text" D.string)
        authorIdDecoder
        (D.maybeField "username" D.string)
        (D.optionField "files" (D.list sFileDecoder) [])
        (D.optionField "attachements" (D.list attachmentDecoder) [])


tsDecoder : Decoder Ts
tsDecoder =
    D.oneOf
        [ D.tagged2 "Ts" Ts D.string (D.map Time.millisToPosix D.int)

        -- From Slack API
        , D.do D.string <|
            \tsStr ->
                case String.toFloat tsStr of
                    Just seconds ->
                        -- ts values are only valid as timestamps to seconds. Decimal values are "uniqifiers"
                        D.succeed (Ts tsStr (Time.millisToPosix (floor seconds * 1000)))

                    Nothing ->
                        D.fail "Invalid `ts` value"
        ]


authorIdDecoder : Decoder AuthorId
authorIdDecoder =
    D.oneOf
        [ D.field "authorId" <| D.tagged "UAuthorId" UAuthorId userIdDecoder
        , D.field "authorId" <| D.tagged "BAuthorId" BAuthorId botIdDecoder

        -- From Slack API
        , D.field "bot_id" (D.map BAuthorId botIdDecoder)
        , D.field "user" (D.map UAuthorId userIdDecoder)
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
        (D.field "text" D.string)
        (D.maybeField "image_url" D.url)
        (D.maybeField "thumb_url" D.url)
        (D.field "fallback" D.string)


colorDecoder : Decoder Element.Color
colorDecoder =
    D.oneOf
        [ D.color

        -- From Slack API
        , D.do D.string <|
            \str ->
                case str of
                    "good" ->
                        D.succeed aubergine.succ

                    "warning" ->
                        D.succeed aubergine.warn

                    "danger" ->
                        D.succeed aubergine.err

                    _ ->
                        if String.startsWith "#" str then
                            D.hexColor (String.dropLeft 1 str)

                        else
                            D.hexColor str
        ]


attachmentAuthorDecoder : Decoder AttachmentAuthor
attachmentAuthorDecoder =
    D.oneOf
        [ D.field "author" <|
            D.map3 AttachmentAuthor
                (D.field "name" D.string)
                (D.field "link" (D.maybe D.url))
                (D.field "icon" (D.maybe D.url))

        -- From Slack API
        , D.map3 AttachmentAuthor
            (D.field "author_name" D.string)
            (D.maybeField "author_link" D.url)
            (D.maybeField "author_icon" D.url)

        -- Deprecated or certain app only?
        , D.map3 AttachmentAuthor
            (D.field "service_name" D.string)
            (D.maybeField "service_url" D.url)
            (D.maybeField "service_icon" D.url)
        ]


attachmentTitleDecoder : Decoder AttachmentTitle
attachmentTitleDecoder =
    D.oneOf
        [ D.field "title" <|
            D.map2 AttachmentTitle (D.field "name" D.string) (D.maybeField "link" D.url)

        -- From Slack API
        , D.map2 AttachmentTitle (D.field "title" D.string) (D.maybeField "title_link" D.url)
        ]


famDecoder : Decoder FAM
famDecoder =
    D.do (D.field "teams" (D.dict teamDecoder)) <|
        \teams ->
            D.map2 FAM
                (D.field "default" Data.Filter.filterAtomDecoder)
                (D.field "conversations" (D.list (conversationCacheDecoder teams)))



-- Component


type alias Yield =
    Producer.Yield () FAM Msg


reload : SlackRegistry -> ( SlackRegistry, Yield )
reload sr =
    ( sr, yield )
        |> reloadUnidentified
        |> reloadAllTeam


reloadUnidentified : ( SlackRegistry, Yield ) -> ( SlackRegistry, Yield )
reloadUnidentified ( sr, y ) =
    case sr.unidentified of
        TokenWritable _ ->
            ( sr, y )

        TokenIdentifying token ->
            ( sr, { y | cmd = Cmd.batch [ y.cmd, identify token ] } )


reloadAllTeam : ( SlackRegistry, Yield ) -> ( SlackRegistry, Yield )
reloadAllTeam ( sr, y ) =
    ( sr, Dict.foldl reloadTeam y sr.dict )


reloadTeam : TeamIdStr -> Slack -> Yield -> Yield
reloadTeam _ slack y =
    case slack of
        Identified { token, team } ->
            -- Saved during hydrate? Retry
            { y | cmd = Cmd.batch [ y.cmd, hydrate token team.id ] }

        Revisit pov ->
            { y
                | cmd = Cmd.batch [ y.cmd, revisit pov.token pov.user.id pov.team.id ]
                , updateFAM = calculateFAM pov.conversations
            }

        _ ->
            -- Other states should not come from IndexedDB
            y


calculateFAM : Dict ConversationIdStr Conversation -> Producer.UpdateFAM FAM
calculateFAM convs =
    -- TODO
    KeepFAM


type Msg
    = -- Prefix "U" means Msg for Unidentified token
      UTokenInput String
    | UTokenCommit
    | UAPIFailure RpcFailure
    | Identify User Team
      -- Prefix "I" means Msg for identified token/team
    | IHydrate TeamIdStr (Dict UserIdStr User) (Dict ConversationIdStr Conversation)
    | IRehydrate TeamIdStr
    | IRevisit TeamIdStr POV
    | ISubscribe TeamIdStr ConversationIdStr
    | IUnsubscribe TeamIdStr ConversationIdStr
    | Fetch Posix -- Fetch is shared across Teams
    | IFetched TeamIdStr FetchSuccess
    | ITokenInput TeamIdStr String
    | ITokenCommit TeamIdStr
    | IAPIFailure TeamIdStr (Maybe ConversationIdStr) RpcFailure


type alias FetchSuccess =
    { conversationId : ConversationIdStr, messages : List (), posix : Posix }


update : Msg -> SlackRegistry -> ( SlackRegistry, Yield )
update msg sr =
    case msg of
        UTokenInput t ->
            pure { sr | unidentified = uTokenInput t sr.unidentified }

        UTokenCommit ->
            handleUTokenCommit sr

        UAPIFailure f ->
            handleUAPIFailure f sr

        Identify user team ->
            handleIdentify user team team.id sr

        IHydrate teamIdStr users conversations ->
            withTeam teamIdStr sr <| handleIHydrate users conversations

        IRehydrate teamIdStr ->
            withTeam teamIdStr sr handleIRehydrate

        IRevisit teamIdStr pov ->
            withTeam teamIdStr sr <| handleIRevisit pov

        ISubscribe teamIdStr convIdStr ->
            withTeam teamIdStr sr <| handleISubscribe convIdStr

        IUnsubscribe teamIdStr convIdStr ->
            withTeam teamIdStr sr <| handleIUnsubscribe convIdStr

        Fetch posix ->
            handleFetch posix sr

        IFetched teamIdStr fetchSucc ->
            withTeam teamIdStr sr <| handleIFetched fetchSucc

        ITokenInput teamIdStr token ->
            withTeam teamIdStr sr <| handleITokenInput token

        ITokenCommit teamIdStr ->
            withTeam teamIdStr sr handleITokenCommit

        IAPIFailure teamIdStr convIdMaybe f ->
            withTeam teamIdStr sr <| handleIAPIFailure convIdMaybe f


uTokenInput : String -> SlackUnidentified -> SlackUnidentified
uTokenInput t su =
    case su of
        TokenWritable _ ->
            TokenWritable t

        TokenIdentifying _ ->
            -- Cannot overwrite
            su


handleUTokenCommit : SlackRegistry -> ( SlackRegistry, Yield )
handleUTokenCommit sr =
    case sr.unidentified of
        TokenWritable "" ->
            pure sr

        TokenWritable token ->
            ( { sr | unidentified = TokenIdentifying token }
            , { yield | cmd = identify token }
            )

        TokenIdentifying _ ->
            pure sr


handleUAPIFailure : RpcFailure -> SlackRegistry -> ( SlackRegistry, Yield )
handleUAPIFailure f sr =
    case sr.unidentified of
        TokenWritable _ ->
            pure sr

        TokenIdentifying t ->
            -- Identify failure; back to input
            pure { sr | unidentified = TokenWritable t }


handleIdentify : User -> Team -> TeamId -> SlackRegistry -> ( SlackRegistry, Yield )
handleIdentify user team (TeamId teamIdStr) sr =
    case sr.unidentified of
        TokenWritable _ ->
            -- Should not happen
            pure sr

        TokenIdentifying token ->
            let
                initTeam t =
                    ( { dict = Dict.insert teamIdStr (Identified (NewSession t user team)) sr.dict
                      , unidentified = TokenWritable ""
                      }
                    , { yield | cmd = hydrate token (TeamId teamIdStr) }
                    )
            in
            case Dict.get teamIdStr sr.dict of
                Just (Identified _) ->
                    initTeam token

                Just _ ->
                    -- Should not happen. See handleIRevisit for Revisit
                    pure sr

                Nothing ->
                    initTeam token


withTeam : TeamIdStr -> SlackRegistry -> (Slack -> ( Slack, Yield )) -> ( SlackRegistry, Yield )
withTeam teamIdStr sr func =
    case Dict.get teamIdStr sr.dict of
        Just slack ->
            let
                ( newSlack, y ) =
                    func slack
            in
            ( { sr | dict = Dict.insert teamIdStr newSlack sr.dict }, y )

        Nothing ->
            pure sr


handleIHydrate : Dict UserIdStr User -> Dict ConversationIdStr Conversation -> Slack -> ( Slack, Yield )
handleIHydrate users convs slack =
    case slack of
        Identified { token, user, team } ->
            ( Hydrated token (initPov token user team convs users)
            , { yield | persist = True, work = Just Worque.SlackFetch }
            )

        Rehydrating token pov ->
            let
                newConvs =
                    mergeConversations pov.conversations convs
            in
            ( Hydrated token { pov | users = users, conversations = newConvs }
            , { yield | persist = True, updateFAM = calculateFAM newConvs }
            )

        _ ->
            -- Should not happen. See handleIRevisit for Revisit
            pure slack


initPov : String -> User -> Team -> Dict ConversationIdStr Conversation -> Dict UserIdStr User -> POV
initPov token user team convs users =
    { token = token
    , user = user
    , team = team
    , conversations = convs
    , users = users
    , bots = Dict.empty
    }


mergeConversations : Dict ConversationIdStr Conversation -> Dict ConversationIdStr Conversation -> Dict ConversationIdStr Conversation
mergeConversations oldConvs newConvs =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/kicked) Conversation
            acc

        foundInBoth cId old new acc =
            -- Use old's cursor (lastRead), let our polling naturally catch up
            -- Note that the new conversation may change to different variant (e.g. PublicChannel => PrivateChannel)
            Dict.insert cId { new | lastRead = old.lastRead, fetchStatus = old.fetchStatus } acc

        foundOnlyInNew cId new acc =
            Dict.insert cId new acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldConvs newConvs Dict.empty


handleIRehydrate : Slack -> ( Slack, Yield )
handleIRehydrate slack =
    case slack of
        Hydrated token pov ->
            -- Rehydrate should only be available in Hydrated state
            ( Rehydrating token pov, { yield | cmd = hydrate pov.token pov.team.id } )

        _ ->
            pure slack


handleIRevisit : POV -> Slack -> ( Slack, Yield )
handleIRevisit pov slack =
    let
        hydrateWithNewPov work oldPov =
            let
                newConvs =
                    mergeConversations oldPov.conversations pov.conversations
            in
            ( Hydrated pov.token { pov | conversations = newConvs }
            , { yield | persist = True, updateFAM = calculateFAM newConvs, work = work }
            )
    in
    case slack of
        Hydrated _ oldPov ->
            -- Replace token
            hydrateWithNewPov Nothing oldPov

        Revisit oldPov ->
            hydrateWithNewPov (Just Worque.SlackFetch) oldPov

        Expired _ oldPov ->
            hydrateWithNewPov (Just Worque.SlackFetch) oldPov

        _ ->
            -- Should not happen
            pure slack


handleISubscribe : ConversationIdStr -> Slack -> ( Slack, Yield )
handleISubscribe convIdStr slack =
    case slack of
        Hydrated token pov ->
            subscribeImpl (Hydrated token) convIdStr pov

        Rehydrating token pov ->
            subscribeImpl (Hydrated token) convIdStr pov

        _ ->
            -- Otherwise not allowed (invluding Revisit)
            pure slack


subscribeImpl : (POV -> Slack) -> ConversationIdStr -> POV -> ( Slack, Yield )
subscribeImpl tagger convIdStr pov =
    withConversation tagger convIdStr pov Nothing <|
        updateFetchStatus FetchStatus.Sub


type alias ConvYield =
    { persist : Bool
    , items : List () -- Must be sorted from oldest to latest (Broker-ready)
    , updateFAM : Bool
    }


withConversation :
    (POV -> Slack)
    -> ConversationIdStr
    -> POV
    -> Maybe Worque.Work
    -> (Conversation -> ( Conversation, ConvYield ))
    -> ( Slack, Yield )
withConversation tagger convIdStr pov work func =
    case Dict.get convIdStr pov.conversations of
        Just conv ->
            let
                ( newConv, cy ) =
                    func conv

                newConvs =
                    Dict.insert convIdStr newConv pov.conversations
            in
            ( tagger { pov | conversations = newConvs }
            , { yield
                | persist = cy.persist
                , items = cy.items
                , updateFAM = updateOrKeepFAM cy.updateFAM newConvs
                , work = work
              }
            )

        Nothing ->
            -- Conversation somehow gone; should not basically happen
            ( tagger pov
            , { yield
                | persist = True
                , updateFAM = calculateFAM pov.conversations
                , work = work
              }
            )


updateOrKeepFAM : Bool -> Dict ConversationIdStr Conversation -> Producer.UpdateFAM FAM
updateOrKeepFAM doUpdate convs =
    if doUpdate then
        calculateFAM convs

    else
        KeepFAM


updateFetchStatus : FetchStatus.Msg -> Conversation -> ( Conversation, ConvYield )
updateFetchStatus fMsg conv =
    let
        { fs, persist, updateFAM } =
            FetchStatus.update fMsg conv.fetchStatus
    in
    ( { conv | fetchStatus = fs }
    , { persist = persist, items = [], updateFAM = updateFAM }
    )


handleIUnsubscribe : ConversationIdStr -> Slack -> ( Slack, Yield )
handleIUnsubscribe convIdStr slack =
    case slack of
        Hydrated token pov ->
            unsubscribeImpl (Hydrated token) convIdStr pov

        Rehydrating token pov ->
            unsubscribeImpl (Rehydrating token) convIdStr pov

        Expired token pov ->
            unsubscribeImpl (Expired token) convIdStr pov

        _ ->
            -- Otherwise not allowed, including Revisit
            pure slack


unsubscribeImpl : (POV -> Slack) -> ConversationIdStr -> POV -> ( Slack, Yield )
unsubscribeImpl tagger convIdStr pov =
    withConversation tagger convIdStr pov Nothing <|
        updateFetchStatus FetchStatus.Unsub


handleFetch : Posix -> SlackRegistry -> ( SlackRegistry, Yield )
handleFetch posix sr =
    case readyToFetchTeamAndConv posix sr.dict of
        Just ( teamIdStr, conv ) ->
            withTeam teamIdStr sr <| handleFetchImpl posix conv

        Nothing ->
            ( sr, { yield | work = Just Worque.SlackFetch } )


handleFetchImpl : Posix -> Conversation -> Slack -> ( Slack, Yield )
handleFetchImpl posix conv slack =
    let
        ( newConv, _ ) =
            -- We never persist on Start
            updateFetchStatus (FetchStatus.Start posix) conv
    in
    case slack of
        Hydrated t pov ->
            ( Hydrated t { pov | conversations = Dict.insert (getConversationIdStr conv) newConv pov.conversations }
            , { yield | cmd = fetchConversationMessages pov.token pov.team.id conv }
            )

        Rehydrating t pov ->
            ( Rehydrating t { pov | conversations = Dict.insert (getConversationIdStr conv) newConv pov.conversations }
            , { yield | cmd = fetchConversationMessages pov.token pov.team.id conv }
            )

        _ ->
            -- Keep next SlackFetch pushed, since it is shared across Teams.
            -- Single Expired Team should not stop the SlackRegistry as a whole.
            ( slack, { yield | work = Just Worque.SlackFetch } )


readyToFetchTeamAndConv : Posix -> Dict TeamIdStr Slack -> Maybe ( TeamIdStr, Conversation )
readyToFetchTeamAndConv posix dict =
    let
        reducer teamIdStr slack acc =
            case slack of
                Hydrated _ pov ->
                    recusrivelyFindConvToFetch posix teamIdStr (Dict.values pov.conversations) acc

                Rehydrating _ pov ->
                    recusrivelyFindConvToFetch posix teamIdStr (Dict.values pov.conversations) acc

                _ ->
                    acc
    in
    Dict.foldl reducer Nothing dict


recusrivelyFindConvToFetch : Posix -> TeamIdStr -> List Conversation -> Maybe ( TeamIdStr, Conversation ) -> Maybe ( TeamIdStr, Conversation )
recusrivelyFindConvToFetch posix teamIdStr conversations acc =
    case conversations of
        [] ->
            acc

        x :: xs ->
            let
                threshold =
                    case acc of
                        Just ( _, accConv ) ->
                            accConv.fetchStatus

                        Nothing ->
                            NextFetchAt posix FetchStatus.BO10
            in
            if FetchStatus.lessThan threshold x.fetchStatus then
                recusrivelyFindConvToFetch posix teamIdStr xs (Just ( teamIdStr, x ))

            else
                recusrivelyFindConvToFetch posix teamIdStr xs acc


handleIFetched : FetchSuccess -> Slack -> ( Slack, Yield )
handleIFetched { conversationId, messages, posix } slack =
    case slack of
        Hydrated t pov ->
            updatePovOnFetchSuccess (Hydrated t) conversationId messages posix pov

        Rehydrating t pov ->
            updatePovOnFetchSuccess (Rehydrating t) conversationId messages posix pov

        Expired t pov ->
            updatePovOnFetchSuccess (Expired t) conversationId messages posix pov

        _ ->
            -- Should not happen
            pure slack


updatePovOnFetchSuccess : (POV -> Slack) -> ConversationIdStr -> List () -> Posix -> POV -> ( Slack, Yield )
updatePovOnFetchSuccess tagger convIdStr ms posix pov =
    withConversation tagger convIdStr pov (Just Worque.SlackFetch) <|
        case ms of
            [] ->
                updateFetchStatus (FetchStatus.Miss posix)

            () :: _ ->
                \conv ->
                    let
                        ( newConv, cy ) =
                            updateFetchStatus (FetchStatus.Hit posix) conv
                    in
                    -- Expects ms to be sorted from latest to oldest. Reverse it for post-processing.
                    -- TODO update lastRead of newConv
                    -- TODO return `List.reverse ms` when Message type is ready
                    ( newConv, { cy | persist = True, items = [] } )


handleITokenInput : String -> Slack -> ( Slack, Yield )
handleITokenInput token slack =
    case slack of
        Hydrated _ pov ->
            pure (Hydrated token pov)

        Expired _ pov ->
            pure (Expired token pov)

        _ ->
            -- Otherwise not allowed
            pure slack


handleITokenCommit : Slack -> ( Slack, Yield )
handleITokenCommit slack =
    case slack of
        Hydrated newToken pov ->
            ( slack, { yield | cmd = revisit newToken pov.user.id pov.team.id } )

        Expired newToken pov ->
            ( slack, { yield | cmd = revisit newToken pov.user.id pov.team.id } )

        _ ->
            -- Otherwise not allowed
            pure slack


handleIAPIFailure : Maybe ConversationIdStr -> RpcFailure -> Slack -> ( Slack, Yield )
handleIAPIFailure convIdMaybe f slack =
    case slack of
        Identified { token, team } ->
            -- If successfully Identified, basically Hydrate should not fail. Just retry. TODO limit number of retries
            ( slack, { yield | cmd = hydrate token team.id } )

        Hydrated t pov ->
            -- Failure outside of fetch indicates new token tried but invalid. Just restore previous token.
            updatePovOnIApiFailure (Expired t) (Hydrated pov.token) convIdMaybe f pov

        Rehydrating t pov ->
            -- Failure outside of fetch indicates rehydration somehow failed. Just restore previous Hydrated state.
            updatePovOnIApiFailure (Expired t) (Hydrated pov.token) convIdMaybe f pov

        Revisit pov ->
            -- Somehow Revisit failed. Settle with Expired with old pov.
            ( Expired pov.token pov, { yield | persist = True, updateFAM = calculateFAM pov.conversations } )

        Expired _ _ ->
            -- Any API failure on Expired. Just keep the state.
            pure slack


updatePovOnIApiFailure : (POV -> Slack) -> (POV -> Slack) -> Maybe ConversationIdStr -> RpcFailure -> POV -> ( Slack, Yield )
updatePovOnIApiFailure unauthorizedTagger tagger convIdMaybe f pov =
    case ( f, convIdMaybe ) of
        -- Slack API basically returns 200 OK, so we consider HttpFailure as transient
        ( HttpFailure hf, Just fetchedConvIdStr ) ->
            -- Fail on history fetch
            withConversation tagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                updateFetchStatus FetchStatus.Fail

        ( HttpFailure hf, Nothing ) ->
            -- Fail on other
            pure (tagger pov)

        ( RpcError err, Just fetchedConvIdStr ) ->
            if unauthorizedRpc err then
                withConversation unauthorizedTagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                    updateFetchStatus FetchStatus.Fail

            else if conversationUnavailable err then
                let
                    newConvs =
                        Dict.remove fetchedConvIdStr pov.conversations
                in
                ( tagger { pov | conversations = newConvs }
                , { yield | persist = True, work = Just Worque.SlackFetch, updateFAM = calculateFAM newConvs }
                )

            else
                -- Transient otherwise
                withConversation tagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                    updateFetchStatus FetchStatus.Fail

        ( RpcError err, Nothing ) ->
            if unauthorizedRpc err then
                ( unauthorizedTagger pov, { yield | persist = True } )

            else
                -- Transient otherwise
                pure (tagger pov)


unauthorizedRpc : String -> Bool
unauthorizedRpc err =
    case err of
        "not_authed" ->
            True

        "invalid_auth" ->
            True

        "account_inactive" ->
            True

        "token_revoked" ->
            True

        _ ->
            False


conversationUnavailable : String -> Bool
conversationUnavailable err =
    case err of
        "channel_not_found" ->
            True

        "missing_scope" ->
            True

        "no_permission" ->
            True

        _ ->
            False



-- REST API CLIENTS


endpoint : String -> Maybe String -> Url
endpoint path queryMaybe =
    { protocol = Url.Https
    , host = "slack.com"
    , port_ = Nothing
    , path = "/api" ++ path
    , fragment = Nothing
    , query = queryMaybe
    }


type RpcFailure
    = HttpFailure HttpClient.Failure
    | RpcError String


{-| Slack API does not allow CORS with custom headers, so:

  - Must use "token in body"
  - Use (almost always) POST

Some APIs allow `application/json`; use `rpcPostJsonTask`.

-}
rpcPostFormTask : Url -> String -> List ( String, String ) -> Decoder a -> Task RpcFailure a
rpcPostFormTask url token kvPairs dec =
    let
        parts =
            List.map (\( k, v ) -> Http.stringPart k v) (( "token", token ) :: kvPairs)
    in
    HttpClient.postFormWithAuth url parts noAuth (rpcDecoder dec)
        |> Task.mapError HttpFailure
        |> Task.andThen extractRpcError


rpcDecoder : Decoder a -> Decoder (Result RpcFailure a)
rpcDecoder dec =
    D.do (D.field "ok" D.bool) <|
        \isOk ->
            if isOk then
                D.map Ok dec

            else
                D.map (Err << RpcError) (D.field "error" D.string)


extractRpcError : Result RpcFailure a -> Task RpcFailure a
extractRpcError res =
    case res of
        Ok a ->
            Task.succeed a

        Err rpcError ->
            Task.fail rpcError


rpcTry : (a -> Msg) -> (RpcFailure -> Msg) -> Task RpcFailure a -> Cmd Msg
rpcTry succ fail task =
    let
        toMsg res =
            case res of
                Ok a ->
                    succ a

                Err e ->
                    fail e
    in
    Task.attempt toMsg task


identify : String -> Cmd Msg
identify token =
    let
        identifyTask userId =
            Task.map2 Identify
                (userInfoTask token userId)
                (teamInfoTask token)
    in
    authTestTask token
        |> Task.andThen identifyTask
        |> rpcTry identity UAPIFailure


{-| Combines identify and hydrate, used on Revisit or Expired. Not requesting to auth.test.

Hydrate is somewhat cheap in Slack compared to Discord, so do it on every reload.

-}
revisit : String -> UserId -> TeamId -> Cmd Msg
revisit token (UserId userIdStr) (TeamId teamIdStr) =
    userListTask token
        |> Task.andThen
            (\users ->
                case Dict.get userIdStr users of
                    Just user ->
                        Task.map2 (\team convs -> initPov token user team convs users)
                            (teamInfoTask token)
                            (conversationListTask token users)

                    Nothing ->
                        -- Should never happen; requesting user must be included in users
                        Task.fail (RpcError ("Cannot retrieve User: " ++ userIdStr))
            )
        |> rpcTry (IRevisit teamIdStr) (IAPIFailure teamIdStr Nothing)


authTestTask : String -> Task RpcFailure UserId
authTestTask token =
    rpcPostFormTask (endpoint "/auth.test" Nothing) token [] <|
        D.field "user_id" userIdDecoder


userInfoTask : String -> UserId -> Task RpcFailure User
userInfoTask token (UserId userId) =
    rpcPostFormTask (endpoint "/users.info" Nothing) token [ ( "user", userId ) ] <|
        D.field "user" userDecoder


teamInfoTask : String -> Task RpcFailure Team
teamInfoTask token =
    rpcPostFormTask (endpoint "/team.info" Nothing) token [] <|
        D.field "team" teamDecoder


hydrate : String -> TeamId -> Cmd Msg
hydrate token (TeamId teamIdStr) =
    userListTask token
        |> Task.andThen
            (\users ->
                Task.map (IHydrate teamIdStr users) (conversationListTask token users)
            )
        |> rpcTry identity (IAPIFailure teamIdStr Nothing)


conversationListTask : String -> Dict UserIdStr User -> Task RpcFailure (Dict ConversationIdStr Conversation)
conversationListTask token users =
    rpcPostFormTask (endpoint "/conversations.list" Nothing)
        token
        [ ( "types", "public_channel,private_channel,im,mpim" ) ]
        (D.field "channels" (D.dictFromList getConversationIdStr (conversationDecoder users)))


userListTask : String -> Task RpcFailure (Dict UserIdStr User)
userListTask token =
    let
        listDecoder =
            D.field "members" (D.list userDecoder)

        toStr (UserId userIdStr) =
            userIdStr
    in
    -- `users.list` does not have default `limit`
    forwardScrollingPostFormTask (endpoint "/users.list" Nothing) token [] listDecoder Initial
        |> Task.map (List.foldl (\u accDict -> Dict.insert (toStr u.id) u accDict) Dict.empty)


fetchConversationMessages : String -> TeamId -> Conversation -> Cmd Msg
fetchConversationMessages token (TeamId teamIdStr) conv =
    let
        (ConversationId convIdStr) =
            conv.id

        combiner messages posix =
            { conversationId = convIdStr, messages = messages, posix = posix }
    in
    Task.map2 combiner (conversationHistoryTask token convIdStr conv.lastRead Initial) Time.now
        |> rpcTry (IFetched teamIdStr) (IAPIFailure teamIdStr (Just convIdStr))


type CursorIn a
    = Initial
    | CursorIn String (List a)


type CursorOut a
    = CursorOut String (List a)
    | Done (List a)


forwardScrollingPostFormTask : Url -> String -> List ( String, String ) -> Decoder (List a) -> CursorIn a -> Task RpcFailure (List a)
forwardScrollingPostFormTask url token params dec cursorIn =
    let
        scrollOrDone acc cursorOut =
            case cursorOut of
                CursorOut cursor new ->
                    forwardScrollingPostFormTask url token params dec (CursorIn cursor (new ++ acc))

                Done new ->
                    Task.succeed (new ++ acc)
    in
    case cursorIn of
        Initial ->
            rpcPostFormTask url token params (scrollableDecoder dec) |> Task.andThen (scrollOrDone [])

        CursorIn c acc ->
            rpcPostFormTask url token (( "cursor", c ) :: params) (scrollableDecoder dec) |> Task.andThen (scrollOrDone acc)


scrollableDecoder : Decoder (List a) -> Decoder (CursorOut a)
scrollableDecoder dec =
    D.map2 (|>) dec <|
        D.oneOf
            [ D.do (D.at [ "response_metadata", "next_cursor" ] D.string) <|
                \c ->
                    case c of
                        "" ->
                            D.fail "Cursor empty"

                        _ ->
                            D.succeed (CursorOut c)
            , D.succeed Done
            ]


conversationHistoryTask : String -> ConversationIdStr -> Maybe LastRead -> CursorIn () -> Task RpcFailure (List ())
conversationHistoryTask token convIdStr lrMaybe cursorIn =
    let
        baseParams =
            -- Recommended to be no more than 200; https://api.slack.com/methods/conversations.history
            [ ( "channel", convIdStr ), ( "limit", "200" ) ]

        baseDecoder =
            -- TODO use messageDecoder when ready
            D.field "messages" (D.leakyList (D.succeed ()))

        url =
            endpoint conversationHistoryPath Nothing
    in
    case lrMaybe of
        Just (LastRead (Ts lastReadTs _)) ->
            forwardScrollingPostFormTask url token (( "oldest", lastReadTs ) :: baseParams) baseDecoder Initial

        Nothing ->
            -- Means never fetched. Without `latest`, it fetches up to current time,
            -- and the first segment returned IS the latest one. (No need to scrolling backward)
            rpcPostFormTask url token baseParams baseDecoder


conversationHistoryPath : String
conversationHistoryPath =
    "/conversations.history"



-- Runtime APIs


getUser : Dict UserIdStr User -> UserId -> Result UserIdStr User
getUser users (UserId userIdStr) =
    case Dict.get userIdStr users of
        Just u ->
            Ok u

        Nothing ->
            Err userIdStr


getConversationFromCache : ConversationIdStr -> List ConversationCache -> Maybe ConversationCache
getConversationFromCache convIdStr caches =
    ListExtra.findOne (\c -> c.id == ConversationId convIdStr) caches


isChannel : Conversation -> Bool
isChannel conv =
    case conv.type_ of
        PublicChannel _ ->
            True

        PrivateChannel ->
            True

        IM ->
            False

        MPIM ->
            False


compareByMembersipThenName : Conversation -> Conversation -> Order
compareByMembersipThenName conv1 conv2 =
    if conv1 == conv2 then
        EQ

    else
        let
            compareToPub isMember =
                if isMember then
                    GT

                else
                    LT
        in
        case ( conv1.type_, conv2.type_ ) of
            ( PublicChannel isMemberA, PublicChannel isMemberB ) ->
                case ( isMemberA, isMemberB ) of
                    ( True, False ) ->
                        LT

                    ( False, True ) ->
                        GT

                    _ ->
                        compare conv1.name conv2.name

            ( PublicChannel True, _ ) ->
                LT

            ( PublicChannel False, _ ) ->
                GT

            ( PrivateChannel, PublicChannel isMember ) ->
                compareToPub isMember

            ( PrivateChannel, PrivateChannel ) ->
                compare conv1.name conv2.name

            ( PrivateChannel, _ ) ->
                LT

            ( IM, PublicChannel isMember ) ->
                compareToPub isMember

            ( IM, IM ) ->
                compare conv1.name conv2.name

            ( IM, MPIM ) ->
                LT

            ( IM, _ ) ->
                GT

            ( MPIM, PublicChannel isMember ) ->
                compareToPub isMember

            ( MPIM, MPIM ) ->
                compare conv1.name conv2.name

            ( MPIM, _ ) ->
                GT


getConversationIdStr : Conversation -> ConversationIdStr
getConversationIdStr conv =
    let
        (ConversationId convIdStr) =
            conv.id
    in
    convIdStr


defaultIconUrl : Maybe Int -> String
defaultIconUrl sizeMaybe =
    logoCdnUrl sizeMaybe "/osogig-6gybeo-d2hu58/Slack%20App%20Icon.png"


logoCdnUrl : Maybe Int -> String -> String
logoCdnUrl sizeMaybe path =
    let
        query =
            case sizeMaybe of
                Just size ->
                    "?width=" ++ String.fromInt size

                Nothing ->
                    ""
    in
    "https://cdn.brandfolder.io/5H442O3W/as" ++ path ++ query


teamUrl : Team -> Url
teamUrl team =
    { protocol = Url.Https
    , host = team.domain ++ ".slack.com"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


{-| Only for testing.
-}
dummyConversationId : ConversationId
dummyConversationId =
    ConversationId "CDUMMYID"
