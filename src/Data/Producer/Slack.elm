module Data.Producer.Slack exposing
    ( Slack(..), SlackUnidentified(..), SlackRegistry, User, Team
    , initRegistry, encodeRegistry, registryDecoder
    , encodeUser, userDecoder, encodeTeam, teamDecoder
    , Msg(..), RpcFailure(..), update
    , defaultIconUrl, teamUrl
    )

{-| Producer for Slack workspaces.

Slack API uses HTTP RPC style. See here for available methods:
<https://api.slack.com/methods>

@docs Slack, SlackUnidentified, SlackRegistry, User, Team
@docs initRegistry, encodeRegistry, registryDecoder
@docs encodeUser, userDecoder, encodeTeam, teamDecoder
@docs Msg, RpcFailure, update
@docs defaultIconUrl, teamUrl

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
import Task exposing (Task)
import Url exposing (Url)


{-| A primary state machine of Slack team-identity combination.

Likely we require a legacy token, but could use a user token in the future.
<https://api.slack.com/custom-integrations/legacy-tokens>
First we get user ID and team ID from `auth.test` API,
<https://api.slack.com/methods/auth.test>
then fetch User and Team using `users.info` and `team.info`, populating `NewSession`.

-}
type Slack
    = Identified NewSession


type alias NewSession =
    { token : String
    , user : User
    , team : Team
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
    = UserId String


type alias UserProfile =
    { email : String
    , displayName : String
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


encodeSession : NewSession -> E.Value
encodeSession session =
    E.object
        [ ( "token", E.string session.token )
        , ( "user", encodeUser session.user )
        , ( "team", encodeTeam session.team )
        ]


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "id", encodeUserId user.id )
        , ( "team_id", encodeTeamId user.teamId )
        , Tuple.pair "profile" <|
            E.object
                [ ( "email", E.string user.profile.email )
                , ( "display_name", E.string user.profile.displayName )
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
        [ D.tagged "Identified" Identified sessionDecoder ]


sessionDecoder : Decoder NewSession
sessionDecoder =
    D.map3 NewSession
        (D.field "token" D.string)
        (D.field "user" userDecoder)
        (D.field "team" teamDecoder)


userDecoder : Decoder User
userDecoder =
    let
        profileDecoder =
            D.map7 UserProfile
                (D.field "email" D.string)
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
            D.map4 TeamIcon
                (D.field "image_34" D.url)
                (D.field "image_44" D.url)
                (D.field "image_68" D.url)
                (D.field "image_default" D.bool)
    in
    D.map4 Team
        (D.field "id" teamIdDecoder)
        (D.field "name" D.string)
        (D.field "domain" D.string)
        (D.field "icon" iconDecoder)



-- Component


type alias Yield =
    Producer.Yield () () Msg


type Msg
    = -- Prefix "U" means Msg for Unidentified token
      UTokenInput String
    | UTokenCommit
    | UAPIFailure RpcFailure
    | Identify User Team


update : Msg -> SlackRegistry -> ( SlackRegistry, Yield )
update msg sr =
    case msg of
        UTokenInput t ->
            pure { sr | unidentified = uTokenInput t sr.unidentified }

        UTokenCommit ->
            uTokenCommit sr

        UAPIFailure f ->
            uAPIFailure f sr

        Identify user team ->
            handleIdentify user team team.id sr


uTokenInput : String -> SlackUnidentified -> SlackUnidentified
uTokenInput t su =
    case su of
        TokenWritable _ ->
            TokenWritable t

        TokenIdentifying _ ->
            -- Cannot overwritten
            su


uTokenCommit : SlackRegistry -> ( SlackRegistry, Yield )
uTokenCommit sr =
    case sr.unidentified of
        TokenWritable "" ->
            pure sr

        TokenWritable token ->
            ( { sr | unidentified = TokenIdentifying token }
            , { yield | cmd = identify token }
            )

        TokenIdentifying _ ->
            pure sr


uAPIFailure : RpcFailure -> SlackRegistry -> ( SlackRegistry, Yield )
uAPIFailure f sr =
    case sr.unidentified of
        TokenWritable _ ->
            pure sr

        TokenIdentifying t ->
            -- Identify failure; back to input
            pure { sr | unidentified = TokenWritable t }


handleIdentify : User -> Team -> TeamId -> SlackRegistry -> ( SlackRegistry, Yield )
handleIdentify user team (TeamId teamId) sr =
    case sr.unidentified of
        TokenWritable _ ->
            -- Should not happen
            pure sr

        TokenIdentifying token ->
            let
                initTeam t =
                    -- TODO retrieve channel list
                    ( { dict = Dict.insert teamId (Identified (NewSession t user team)) sr.dict
                      , unidentified = TokenWritable ""
                      }
                    , { yield | persist = True }
                    )
            in
            case Dict.get teamId sr.dict of
                Just (Identified _) ->
                    initTeam token

                Nothing ->
                    initTeam token



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



-- Logo CDN URLs


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
