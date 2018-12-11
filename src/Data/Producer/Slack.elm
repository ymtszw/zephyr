module Data.Producer.Slack exposing
    ( Slack(..), SlackUnidentified(..), SlackRegistry, User, Team, Conversation(..)
    , initRegistry, encodeRegistry, registryDecoder
    , encodeUser, userDecoder, encodeTeam, teamDecoder, encodeConversation, conversationDecoder
    , Msg(..), RpcFailure(..), reload, update
    , getUser, isChannel, compareByMembersipThenName, conversationFilter
    , defaultIconUrl, teamUrl, dummyConversationId, dummyUserId
    )

{-| Producer for Slack workspaces.

Slack API uses HTTP RPC style. See here for available methods:
<https://api.slack.com/methods>

@docs Slack, SlackUnidentified, SlackRegistry, User, Team, Conversation
@docs initRegistry, encodeRegistry, registryDecoder
@docs encodeUser, userDecoder, encodeTeam, teamDecoder, encodeConversation, conversationDecoder
@docs Msg, RpcFailure, reload, update
@docs getUser, isChannel, compareByMembersipThenName, conversationFilter
@docs defaultIconUrl, teamUrl, dummyConversationId, dummyUserId

-}

import Data.Filter exposing (FilterAtom)
import Data.Producer as Producer exposing (..)
import Dict exposing (Dict)
import Http
import HttpClient exposing (noAuth)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import StringExtra
import Task exposing (Task)
import Url exposing (Url)


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
    { displayName : String
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

TODO: consider how to support IM/MPIMs, while aligning with Discord DM/GroupDM

-}
type Conversation
    = PublicChannel PublicChannelRecord
    | PrivateChannel PrivateChannelRecord
    | IM IMRecord -- Instant Messages, presumably
    | MPIM MPIMRecord -- Multi-person IM


type alias PublicChannelRecord =
    { id : ConversationId, name : String, isMember : Bool, lastRead : Maybe Float }


type alias PrivateChannelRecord =
    { id : ConversationId, name : String, lastRead : Maybe Float }


type alias IMRecord =
    -- For IM, `last_read` is not supplied from conversation object.
    -- But it IS possible to acquire `ts` from messages in conversations.history,
    -- then use it in `oldest` parameter for later requests
    { id : ConversationId, user : UserId, lastRead : Maybe Float }


type alias MPIMRecord =
    { id : ConversationId, name : String, lastRead : Maybe Float }


type ConversationId
    = ConversationId ConversationIdStr


type alias ConversationIdStr =
    String


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



-- Encode


encodeRegistry : SlackRegistry -> E.Value
encodeRegistry sr =
    E.object
        [ ( "dict", E.dict identity encodeSlack sr.dict )
        , ( "unidentified", encodeUnidentified sr.unidentified )
        ]


encodeUnidentified : SlackUnidentified -> E.Value
encodeUnidentified v =
    case v of
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
            -- TODO: add Revisit
            E.tagged2 "Hydrated" (E.string pov.token) (encodePov pov)

        Rehydrating _ pov ->
            -- TODO: add Revisit
            E.tagged2 "Hydrated" (E.string pov.token) (encodePov pov)


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
        ]


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "id", encodeUserId user.id )
        , ( "team_id", encodeTeamId user.teamId )
        , Tuple.pair "profile" <|
            E.object
                [ ( "display_name", E.string user.profile.displayName )
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
    case conv of
        PublicChannel record ->
            E.tagged "PublicChannel" <|
                E.object
                    [ ( "id", encodeConversationId record.id )
                    , ( "name", E.string record.name )
                    , ( "is_member", E.bool record.isMember )
                    , ( "last_read", E.maybe E.float record.lastRead )
                    ]

        PrivateChannel record ->
            E.tagged "PrivateChannel" <|
                E.object
                    [ ( "id", encodeConversationId record.id )
                    , ( "name", E.string record.name )
                    , ( "last_read", E.maybe E.float record.lastRead )
                    ]

        IM record ->
            E.tagged "IM" <|
                E.object
                    [ ( "id", encodeConversationId record.id )
                    , ( "user", encodeUserId record.user )
                    , ( "last_read", E.maybe E.float record.lastRead )
                    ]

        MPIM record ->
            E.tagged "MPIM" <|
                E.object
                    [ ( "id", encodeConversationId record.id )
                    , ( "name", E.string record.name )
                    , ( "last_read", E.maybe E.float record.lastRead )
                    ]


encodeConversationId : ConversationId -> E.Value
encodeConversationId (ConversationId convId) =
    E.tagged "ConversationId" (E.string convId)



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
        , D.tagged2 "Hydrated" Hydrated D.string povDecoder
        ]


sessionDecoder : Decoder NewSession
sessionDecoder =
    D.map3 NewSession
        (D.field "token" D.string)
        (D.field "user" userDecoder)
        (D.field "team" teamDecoder)


povDecoder : Decoder POV
povDecoder =
    D.map5 POV
        (D.field "token" D.string)
        (D.field "user" userDecoder)
        (D.field "team" teamDecoder)
        (D.field "conversations" (D.dict conversationDecoder))
        (D.field "users" (D.dict userDecoder))


userDecoder : Decoder User
userDecoder =
    let
        profileDecoder =
            D.map6 UserProfile
                (D.field "display_name" D.string)
                (D.field "real_name" D.string)
                (D.field "status_text" D.string)
                (D.field "status_emoji" D.string)
                (D.field "image_32" D.url)
                (D.field "image_48" D.url)
    in
    D.map3 User
        (D.field "id" userIdDecoder)
        (D.field "team_id" teamIdDecoder)
        (D.field "profile" profileDecoder)


userIdDecoder : Decoder UserId
userIdDecoder =
    D.oneOf [ D.tagged "UserId" UserId D.string, D.map UserId D.string ]


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


conversationDecoder : Decoder Conversation
conversationDecoder =
    -- XXX We are including archived channels and IM with deleted users.
    let
        pubDecoder =
            D.map4 PublicChannelRecord
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.optionField "is_member" D.bool False)
                (D.maybeField "last_read" D.float)

        privDecoder =
            D.map3 PrivateChannelRecord
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.maybeField "last_read" D.float)

        imDecoder =
            D.map3 IMRecord
                (D.field "id" conversationIdDecoder)
                (D.field "user" userIdDecoder)
                (D.maybeField "last_read" D.float)

        mpimDecoder =
            D.map3 MPIMRecord
                (D.field "id" conversationIdDecoder)
                (D.field "name" D.string)
                (D.maybeField "last_read" D.float)
    in
    D.oneOf
        [ -- From IndexedDB
          D.tagged "PublicChannel" PublicChannel pubDecoder
        , D.tagged "PrivateChannel" PrivateChannel privDecoder
        , D.tagged "IM" IM imDecoder
        , D.tagged "MPIM" MPIM mpimDecoder

        -- From Slack API
        , D.when (D.field "is_mpim" D.bool) identity (D.map MPIM mpimDecoder)
        , D.when (D.field "is_im" D.bool) identity (D.map IM imDecoder)
        , D.oneOf
            [ D.when (D.field "is_group" D.bool) identity (D.map PrivateChannel privDecoder)

            -- The doc says it is a public channel when is_channel: true but it is possible to be paired with is_private: true
            , D.when (D.map2 (&&) (D.field "is_channel" D.bool) (D.field "is_private" D.bool))
                identity
                (D.map PrivateChannel privDecoder)
            ]
        , D.when (D.map2 Tuple.pair (D.field "is_channel" D.bool) (D.field "is_private" D.bool))
            ((==) ( True, False ))
            (D.map PublicChannel pubDecoder)
        ]


conversationIdDecoder : Decoder ConversationId
conversationIdDecoder =
    D.oneOf [ D.tagged "ConversationId" ConversationId D.string, D.map ConversationId D.string ]



-- Component


type alias Yield =
    Producer.Yield () () Msg


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

        Hydrated _ pov ->
            -- TODO: Add Revisit => re-Identify
            { y | updateFAM = calculateFAM pov.conversations }

        _ ->
            -- Other states should not come from IndexedDB
            y


calculateFAM : Dict ConversationIdStr Conversation -> Producer.UpdateFAM ()
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
    | IHydrate TeamIdStr (Dict ConversationIdStr Conversation) (Dict UserIdStr User)
    | IRehydrate TeamIdStr
    | IAPIFailure TeamIdStr RpcFailure


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

        IHydrate teamIdStr conversations users ->
            handleIHydrate teamIdStr conversations users sr

        IRehydrate teamIdStr ->
            handleIRehydrate teamIdStr sr

        IAPIFailure teamIdStr f ->
            handleIAPIFailure teamIdStr f sr


uTokenInput : String -> SlackUnidentified -> SlackUnidentified
uTokenInput t su =
    case su of
        TokenWritable _ ->
            TokenWritable t

        TokenIdentifying _ ->
            -- Cannot overwritten
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
                    -- Should not happen; TODO Revisit => re-Identify
                    pure sr

                Nothing ->
                    initTeam token


handleIHydrate :
    TeamIdStr
    -> Dict ConversationIdStr Conversation
    -> Dict UserIdStr User
    -> SlackRegistry
    -> ( SlackRegistry, Yield )
handleIHydrate teamIdStr convs users sr =
    case Dict.get teamIdStr sr.dict of
        Just (Identified { token, user, team }) ->
            let
                slack =
                    Hydrated token { token = token, user = user, team = team, conversations = convs, users = users }
            in
            ( { sr | dict = Dict.insert teamIdStr slack sr.dict }, { yield | persist = True } )

        Just (Rehydrating token pov) ->
            let
                newConvs =
                    mergeConversations pov.conversations convs

                slack =
                    Hydrated token { pov | users = users, conversations = newConvs }
            in
            ( { sr | dict = Dict.insert teamIdStr slack sr.dict }
            , { yield | persist = True, updateFAM = calculateFAM newConvs }
            )

        Just _ ->
            -- Should not happen
            pure sr

        Nothing ->
            -- Rehydrate initiated but the Team is discarded? Should not happen.
            pure sr


mergeConversations : Dict ConversationIdStr Conversation -> Dict ConversationIdStr Conversation -> Dict ConversationIdStr Conversation
mergeConversations oldConvs newConvs =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/kicked) Conversation
            acc

        foundInBoth cId old new acc =
            -- TODO Use old's cursor (lastRead) when introduced, let our scan naturally catch up
            -- Note that the new conversation may change to different variant (e.g. PublicChannel => PrivateChannel)
            Dict.insert cId new acc

        foundOnlyInNew cId new acc =
            Dict.insert cId new acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldConvs newConvs Dict.empty


handleIRehydrate : TeamIdStr -> SlackRegistry -> ( SlackRegistry, Yield )
handleIRehydrate teamIdStr sr =
    case Dict.get teamIdStr sr.dict of
        Just (Hydrated token pov) ->
            -- Rehydrate should only be available in Hydrated state
            ( { sr | dict = Dict.insert teamIdStr (Rehydrating token pov) sr.dict }
            , { yield | cmd = hydrate pov.token (TeamId teamIdStr) }
            )

        _ ->
            pure sr


handleIAPIFailure : TeamIdStr -> RpcFailure -> SlackRegistry -> ( SlackRegistry, Yield )
handleIAPIFailure teamIdStr rpcFailure sr =
    case Dict.get teamIdStr sr.dict of
        Just (Identified _) ->
            -- If successfully Identified, basically Hydrate should not fail. Discard the Team.
            ( { sr | dict = Dict.remove teamIdStr sr.dict }, { yield | persist = True } )

        Just (Hydrated _ pov) ->
            -- New token invalid? Just restore token input.
            pure { sr | dict = Dict.insert teamIdStr (Hydrated pov.token pov) sr.dict }

        Just (Rehydrating _ pov) ->
            -- Somehow Rehydrate failed. Just fall back to previous Hydrated state.
            pure { sr | dict = Dict.insert teamIdStr (Hydrated pov.token pov) sr.dict }

        Nothing ->
            -- Late arrival?
            pure sr



-- REST API CLIENTS


apiPath : String -> Maybe String -> Url
apiPath path queryMaybe =
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


authTestTask : String -> Task RpcFailure UserId
authTestTask token =
    rpcPostFormTask (apiPath "/auth.test" Nothing) token [] <|
        D.field "user_id" userIdDecoder


userInfoTask : String -> UserId -> Task RpcFailure User
userInfoTask token (UserId userId) =
    rpcPostFormTask (apiPath "/users.info" Nothing) token [ ( "user", userId ) ] <|
        D.field "user" userDecoder


teamInfoTask : String -> Task RpcFailure Team
teamInfoTask token =
    rpcPostFormTask (apiPath "/team.info" Nothing) token [] <|
        D.field "team" teamDecoder


hydrate : String -> TeamId -> Cmd Msg
hydrate token (TeamId teamIdStr) =
    Task.map2 (IHydrate teamIdStr) (conversationListTask token) (userListTask token)
        |> rpcTry identity (IAPIFailure teamIdStr)


conversationListTask : String -> Task RpcFailure (Dict ConversationIdStr Conversation)
conversationListTask token =
    rpcPostFormTask (apiPath "/conversations.list" Nothing)
        token
        [ ( "types", "public_channel,private_channel,im,mpim" ) ]
        (D.field "channels" (D.dictFromList getConvIdStr conversationDecoder))


getConvIdStr : Conversation -> ConversationIdStr
getConvIdStr conv =
    let
        toStr (ConversationId convIdStr) =
            convIdStr
    in
    case conv of
        PublicChannel { id } ->
            toStr id

        PrivateChannel { id } ->
            toStr id

        IM { id } ->
            toStr id

        MPIM { id } ->
            toStr id


userListTask : String -> Task RpcFailure (Dict UserIdStr User)
userListTask token =
    let
        toStr (UserId userIdStr) =
            userIdStr
    in
    rpcPostFormTask (apiPath "/users.list" Nothing) token [] <|
        D.field "members" (D.dictFromList (.id >> toStr) userDecoder)



-- Runtime APIs


getUser : Dict UserIdStr User -> UserId -> Result UserIdStr User
getUser users (UserId userIdStr) =
    case Dict.get userIdStr users of
        Just u ->
            Ok u

        Nothing ->
            Err userIdStr


isChannel : Conversation -> Bool
isChannel conv =
    case conv of
        PublicChannel _ ->
            True

        PrivateChannel _ ->
            True

        IM _ ->
            False

        MPIM _ ->
            False


compareByMembersipThenName : Conversation -> Conversation -> Order
compareByMembersipThenName convA convB =
    if convA == convB then
        EQ

    else
        let
            compareToPub isMember =
                if isMember then
                    GT

                else
                    LT
        in
        case ( convA, convB ) of
            ( PublicChannel pub1, PublicChannel pub2 ) ->
                case ( pub1.isMember, pub2.isMember ) of
                    ( True, False ) ->
                        LT

                    ( False, True ) ->
                        GT

                    _ ->
                        compare pub1.name pub2.name

            ( PublicChannel pub1, _ ) ->
                if pub1.isMember then
                    LT

                else
                    GT

            ( PrivateChannel _, PublicChannel pub2 ) ->
                compareToPub pub2.isMember

            ( PrivateChannel priv1, PrivateChannel priv2 ) ->
                compare priv1.name priv2.name

            ( PrivateChannel _, _ ) ->
                LT

            ( IM _, PublicChannel pub2 ) ->
                compareToPub pub2.isMember

            ( IM im1, IM im2 ) ->
                let
                    ( UserId u1, UserId u2 ) =
                        ( im1.user, im2.user )
                in
                -- Compare by UserId, not by User's names
                compare u1 u2

            ( IM _, MPIM _ ) ->
                LT

            ( IM _, _ ) ->
                GT

            ( MPIM _, PublicChannel pub2 ) ->
                compareToPub pub2.isMember

            ( MPIM mpim1, MPIM mpim2 ) ->
                compare mpim1.name mpim2.name

            ( MPIM _, _ ) ->
                GT


conversationFilter : Dict UserIdStr User -> String -> Conversation -> Bool
conversationFilter users filter conv =
    case conv of
        PublicChannel { name } ->
            StringExtra.containsCaseIgnored filter name

        PrivateChannel { name } ->
            StringExtra.containsCaseIgnored filter name

        IM { user } ->
            case getUser users user of
                Ok u ->
                    StringExtra.containsCaseIgnored filter u.profile.displayName
                        || StringExtra.containsCaseIgnored filter u.profile.realName

                Err userIdStr ->
                    StringExtra.containsCaseIgnored filter userIdStr

        MPIM { name } ->
            -- XXX use users dict?
            StringExtra.containsCaseIgnored filter name


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


{-| Only for testing.
-}
dummyUserId : String -> UserId
dummyUserId userIdStr =
    UserId userIdStr
