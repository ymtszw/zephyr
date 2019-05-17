module Data.Producer.Slack exposing
    ( Slack(..), SlackUnidentified(..), SlackRegistry, Token, FAM
    , initRegistry, encodeRegistry, registryDecoder, encodeFam, famDecoder
    , Msg(..), RpcFailure(..), reload, update
    , defaultIconUrl, getConvoFromCache
    , fromToken
    )

{-| Producer for Slack workspaces.

Slack API uses HTTP RPC style. See here for available methods:
<https://api.slack.com/methods>

@docs Slack, SlackUnidentified, SlackRegistry, Token, FAM
@docs initRegistry, encodeRegistry, registryDecoder, encodeFam, famDecoder
@docs Msg, RpcFailure, reload, update
@docs getUser, isChannel, isPrivate, getConversationIdStr, getPosix, getTs, getAuthorName
@docs defaultIconUrl, getConvoFromCache
@docs fromToken

-}

import AssocList as Dict exposing (Dict)
import Data.Filter as Filter exposing (FilterAtom(..))
import Data.Producer as Producer exposing (..)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Data.Producer.Slack.Bot as Bot exposing (Bot)
import Data.Producer.Slack.Convo as Convo exposing (Convo)
import Data.Producer.Slack.ConvoCache as ConvoCache exposing (ConvoCache)
import Data.Producer.Slack.Message as Message exposing (Message)
import Data.Producer.Slack.Team as Team exposing (Team)
import Data.Producer.Slack.Ts as Ts exposing (Ts)
import Data.Producer.Slack.User as User exposing (User)
import Extra exposing (doT)
import Http
import HttpClient exposing (noAuth)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import List.Extra
import Markdown.Inline exposing (Inline(..))
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

There are another message posting entities: Bots.
Unlike Users, Bots does not have listing API, so we have to collect their info with multiple requests.
Thankfully, Bots aren't many compared to Users, so it SHOULD be OK.

-}
type Slack
    = Identified NewSession
    | Hydrated Token POV
    | Rehydrating Token POV
    | Revisit POV
    | Expired Token POV


type Token
    = Token String


emptyToken : Token
emptyToken =
    Token ""


fromToken : Token -> String
fromToken (Token t) =
    t


type alias NewSession =
    { token : Token
    , user : User
    , team : Team
    }


type alias POV =
    { token : Token
    , user : User
    , team : Team
    , convos : Dict Convo.Id Convo
    , users : Dict User.Id User
    , bots : Dict Bot.Id Bot -- Lazily populated/updated; TODO can become stale, needs some update trigger
    }


{-| Runtime registry of multiple Slack state machines.
-}
type alias SlackRegistry =
    { dict : Dict Team.Id Slack
    , unidentified : SlackUnidentified
    }


{-| Not yet identified token states.
-}
type SlackUnidentified
    = TokenWritable Token
    | TokenIdentifying Token


initRegistry : SlackRegistry
initRegistry =
    { dict = Dict.empty, unidentified = initUnidentified }


initUnidentified : SlackUnidentified
initUnidentified =
    TokenWritable emptyToken


type alias FAM =
    { default : FilterAtom
    , -- List instead of Dict, should be sorted already
      -- XXX Also, this list is SHARED ACROSS MULTIPLE TEAMS!!!
      convos : List ConvoCache
    }



-- Encode


encodeRegistry : SlackRegistry -> E.Value
encodeRegistry sr =
    E.object
        [ ( "dict", E.assocList Id.to encodeSlack sr.dict )
        , ( "unidentified", encodeUnidentified sr.unidentified )
        ]


encodeUnidentified : SlackUnidentified -> E.Value
encodeUnidentified su =
    case su of
        TokenWritable t ->
            E.tagged "TokenWritable" (E.string (fromToken t))

        TokenIdentifying t ->
            E.tagged "TokenIdentifying" (E.string (fromToken t))


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
        [ ( "token", E.string (fromToken session.token) )
        , ( "user", User.encode session.user )
        , ( "team", Team.encode session.team )
        ]


encodePov : POV -> E.Value
encodePov pov =
    E.object
        [ ( "token", E.string (fromToken pov.token) )
        , ( "user", User.encode pov.user )
        , ( "team", Team.encode pov.team )
        , ( "convos", E.assocList Id.to Convo.encode pov.convos )
        , ( "users", E.assocList Id.to User.encode pov.users )
        , ( "bots", E.assocList Id.to Bot.encode pov.bots )
        ]


encodeFam : FAM -> E.Value
encodeFam fam =
    let
        ( encodedConvos, teams ) =
            unjoinTeamsAndEncodeConvos fam.convos
    in
    E.object
        [ ( "default", Filter.encodeFilterAtom fam.default )
        , ( "convos", E.list identity encodedConvos )
        , ( "teams", E.assocList Id.to Team.encode teams )
        ]


unjoinTeamsAndEncodeConvos : List ConvoCache -> ( List E.Value, Dict Team.Id Team )
unjoinTeamsAndEncodeConvos convos =
    let
        reducer c ( accList, accTeams ) =
            ( ConvoCache.encode c :: accList, Dict.insert (Team.getId c.team) c.team accTeams )
    in
    -- Conserve the order of convos; already sorted
    List.foldr reducer ( [], Dict.empty ) convos



-- Decoder


registryDecoder : Decoder SlackRegistry
registryDecoder =
    D.oneOf
        [ D.map2 SlackRegistry
            (D.field "dict" (D.assocList Id.from slackDecoder))
            (D.field "unidentified" unidentifiedDecoder)
        , D.succeed initRegistry -- fallback
        ]


unidentifiedDecoder : Decoder SlackUnidentified
unidentifiedDecoder =
    D.oneOf
        [ D.tagged "TokenWritable" TokenWritable (D.map Token D.string)
        , D.tagged "TokenIdentifying" TokenIdentifying (D.map Token D.string)
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
        (D.field "token" (D.map Token D.string))
        (D.field "user" User.decoder)
        (D.field "team" Team.decoder)


povDecoder : Decoder POV
povDecoder =
    D.do (D.field "users" (D.assocList Id.from User.decoder)) <|
        \users ->
            let
                convosDecoder =
                    D.oneOf
                        [ D.field "convos" (D.assocList Id.from Convo.decoder)
                        , -- Old format
                          D.field "conversations" (D.assocList Id.from Convo.decoder)
                        ]
            in
            D.map6 POV
                (D.field "token" (D.map Token D.string))
                (D.field "user" User.decoder)
                (D.field "team" Team.decoder)
                convosDecoder
                (D.succeed users)
                (D.optionField "bots" (D.assocList Id.from Bot.decoder) Dict.empty)


famDecoder : Decoder FAM
famDecoder =
    D.do (D.field "teams" (D.assocList Id.from Team.decoder)) <|
        \teams ->
            D.map2 FAM (D.field "default" Filter.filterAtomDecoder) <|
                D.oneOf
                    [ D.field "convos" (D.list (ConvoCache.decoder teams))
                    , --Old format
                      D.field "conversations" (D.list (ConvoCache.decoder teams))
                    ]



-- Component


type alias Yield =
    Producer.Yield Message ConvoCache Msg


{-| Yield from sub component of SlackRegistry (i.e. Team or Convo).

Bubbling up to root registry, then Cmds are batched and the availableSources are calculated.

-}
type alias SubYield =
    { cmd : Cmd Msg
    , persist : Bool
    , items : List Message
    , refreshSources : Bool
    , work : Maybe Worque.Work
    }


sYield : SubYield
sYield =
    { cmd = Cmd.none
    , persist = False
    , items = []
    , refreshSources = False
    , work = Nothing
    }


liftToYield : SubYield -> SlackRegistry -> Yield
liftToYield sy sr =
    { cmd = sy.cmd
    , persist = sy.persist
    , items = sy.items
    , work = sy.work
    , availableSources =
        if sy.refreshSources then
            Just (subscribedConvosAcrossTeams sr.dict)

        else
            Nothing
    }


subscribedConvosAcrossTeams : Dict Team.Id Slack -> List ConvoCache
subscribedConvosAcrossTeams dict =
    let
        reducer _ slack acc =
            let
                accumSubbed team _ convo accInner =
                    if not (Convo.getIsArchived convo) && FetchStatus.subscribed (Convo.getFetchStatus convo) then
                        ConvoCache.from team convo :: accInner

                    else
                        accInner
            in
            case slack of
                Hydrated _ pov ->
                    Dict.foldl (accumSubbed pov.team) acc pov.convos

                Rehydrating _ pov ->
                    Dict.foldl (accumSubbed pov.team) acc pov.convos

                Revisit pov ->
                    Dict.foldl (accumSubbed pov.team) acc pov.convos

                Expired _ pov ->
                    Dict.foldl (accumSubbed pov.team) acc pov.convos

                _ ->
                    acc
    in
    dict |> Dict.foldl reducer [] |> List.sortWith ConvoCache.compare



-- Reload


reload : SlackRegistry -> ( SlackRegistry, Yield )
reload sr =
    let
        cmdU =
            case sr.unidentified of
                TokenWritable _ ->
                    Cmd.none

                TokenIdentifying token ->
                    identify token

        y =
            liftToYield (Dict.foldl reloadTeam sYield sr.dict) sr
    in
    ( sr, { y | cmd = Cmd.batch [ y.cmd, cmdU ] } )


reloadTeam : Team.Id -> Slack -> SubYield -> SubYield
reloadTeam _ slack sy =
    case slack of
        Identified { token, team } ->
            -- Saved during hydrate? Retry
            { sy | cmd = Cmd.batch [ sy.cmd, hydrate token (Team.getId team) ] }

        Revisit pov ->
            { sy
                | cmd = Cmd.batch [ sy.cmd, revisit pov.token pov ]
                , refreshSources = True
            }

        _ ->
            -- Other states should not come from IndexedDB
            sy



-- Update


type Msg
    = -- Prefix "U" means Msg for "unidentified" token
      UTokenInput String
    | UTokenCommit
    | UAPIFailure RpcFailure
    | Identify User Team
      -- Prefix "I" means Msg for "identified" token/team
    | IHydrate Team.Id (Dict User.Id User) (Dict Convo.Id Convo)
    | IRehydrate Team.Id
    | IRevisit Team.Id POV
    | ISubscribe Team.Id Convo.Id
    | IUnsubscribe Team.Id Convo.Id
    | Fetch Posix -- Fetch is shared across Teams
    | IFetched Team.Id FetchSuccess
    | ITokenInput Team.Id String
    | ITokenCommit Team.Id
    | IAPIFailure Team.Id (Maybe Convo.Id) RpcFailure


type alias FetchSuccess =
    { conversationId : Convo.Id
    , messages : List Message
    , users : Dict User.Id User
    , bots : Dict Bot.Id Bot
    , posix : Posix
    }


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
            handleIdentify user team (Team.getId team) sr

        IHydrate teamId users convos ->
            withTeam teamId sr <| handleIHydrate users convos

        IRehydrate teamId ->
            withTeam teamId sr handleIRehydrate

        IRevisit teamId pov ->
            withTeam teamId sr <| handleIRevisit pov

        ISubscribe teamId convoId ->
            withTeam teamId sr <| handleISubscribe convoId

        IUnsubscribe teamId convoId ->
            withTeam teamId sr <| handleIUnsubscribe convoId

        Fetch posix ->
            handleFetch posix sr

        IFetched teamId fetchSucc ->
            withTeam teamId sr <| handleIFetched fetchSucc

        ITokenInput teamId token ->
            withTeam teamId sr <| handleITokenInput token

        ITokenCommit teamId ->
            withTeam teamId sr handleITokenCommit

        IAPIFailure teamId convoIdMaybe f ->
            withTeam teamId sr <| handleIAPIFailure convoIdMaybe f


uTokenInput : String -> SlackUnidentified -> SlackUnidentified
uTokenInput t su =
    case su of
        TokenWritable _ ->
            TokenWritable (Token t)

        TokenIdentifying _ ->
            -- Cannot overwrite
            su


handleUTokenCommit : SlackRegistry -> ( SlackRegistry, Yield )
handleUTokenCommit sr =
    case sr.unidentified of
        TokenWritable token ->
            if token == emptyToken then
                pure sr

            else
                ( { sr | unidentified = TokenIdentifying token }
                , { yield | cmd = identify token }
                )

        TokenIdentifying _ ->
            pure sr


handleUAPIFailure : RpcFailure -> SlackRegistry -> ( SlackRegistry, Yield )
handleUAPIFailure _ sr =
    case sr.unidentified of
        TokenWritable _ ->
            pure sr

        TokenIdentifying t ->
            -- Identify failure; back to input
            pure { sr | unidentified = TokenWritable t }


handleIdentify : User -> Team -> Team.Id -> SlackRegistry -> ( SlackRegistry, Yield )
handleIdentify user team teamId sr =
    case sr.unidentified of
        TokenWritable _ ->
            -- Should not happen
            pure sr

        TokenIdentifying token ->
            let
                initTeam t =
                    ( { dict = Dict.insert teamId (Identified (NewSession t user team)) sr.dict
                      , unidentified = TokenWritable emptyToken
                      }
                    , { yield | cmd = hydrate token teamId }
                    )
            in
            case Dict.get teamId sr.dict of
                Just (Identified _) ->
                    initTeam token

                Just _ ->
                    -- Should not happen. See handleIRevisit for Revisit
                    pure sr

                Nothing ->
                    initTeam token


withTeam : Team.Id -> SlackRegistry -> (Slack -> ( Slack, SubYield )) -> ( SlackRegistry, Yield )
withTeam teamId sr func =
    case Dict.get teamId sr.dict of
        Just slack ->
            let
                ( newSlack, sy ) =
                    func slack

                newSr =
                    { sr | dict = Dict.insert teamId newSlack sr.dict }
            in
            ( newSr, liftToYield sy newSr )

        Nothing ->
            pure sr


handleIHydrate : Dict User.Id User -> Dict Convo.Id Convo -> Slack -> ( Slack, SubYield )
handleIHydrate users convos slack =
    case slack of
        Identified { token, user, team } ->
            ( Hydrated token (initPov token user team convos users)
            , { sYield | persist = True, work = Just Worque.SlackFetch }
            )

        Rehydrating token pov ->
            let
                newConvos =
                    mergeConvos pov.convos convos
            in
            ( Hydrated token { pov | users = users, convos = newConvos }
            , { sYield | persist = True, refreshSources = True }
            )

        _ ->
            -- Should not happen. See handleIRevisit for Revisit
            ( slack, sYield )


initPov : Token -> User -> Team -> Dict Convo.Id Convo -> Dict User.Id User -> POV
initPov token user team convos users =
    { token = token
    , user = user
    , team = team
    , convos = convos
    , users = users
    , bots = Dict.empty
    }


mergeConvos :
    Dict Convo.Id Convo
    -> Dict Convo.Id Convo
    -> Dict Convo.Id Convo
mergeConvos oldConvos newConvos =
    let
        foundOnlyInOld _ _ acc =
            -- Deleting now unreachable (deleted/kicked) Convo
            acc

        foundInBoth cId old new acc =
            -- Use old's cursor (lastRead), let our polling naturally catch up
            -- Note that the new conversation may change to different variant (e.g. PublicChannel => PrivateChannel)
            let
                modified =
                    new
                        |> Convo.setLastRead (Convo.getLastRead old)
                        |> Convo.setFetchStatus (Convo.getFetchStatus old)
            in
            Dict.insert cId modified acc

        foundOnlyInNew cId new acc =
            Dict.insert cId new acc
    in
    Dict.merge foundOnlyInOld foundInBoth foundOnlyInNew oldConvos newConvos Dict.empty


handleIRehydrate : Slack -> ( Slack, SubYield )
handleIRehydrate slack =
    case slack of
        Hydrated token pov ->
            -- Rehydrate should only be available in Hydrated state
            ( Rehydrating token pov, { sYield | cmd = hydrate pov.token (Team.getId pov.team) } )

        _ ->
            ( slack, sYield )


handleIRevisit : POV -> Slack -> ( Slack, SubYield )
handleIRevisit pov slack =
    let
        hydrateWithNewPov work oldPov =
            let
                newConvos =
                    mergeConvos oldPov.convos pov.convos
            in
            ( Hydrated pov.token { pov | convos = newConvos }
            , { sYield | persist = True, refreshSources = True, work = work }
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
            ( slack, sYield )


handleISubscribe : Convo.Id -> Slack -> ( Slack, SubYield )
handleISubscribe convoId slack =
    let
        subscribeImpl tagger pov =
            withConversation tagger convoId pov Nothing <|
                updateFetchStatus FetchStatus.Sub
    in
    case slack of
        Hydrated token pov ->
            subscribeImpl (Hydrated token) pov

        Rehydrating token pov ->
            subscribeImpl (Hydrated token) pov

        _ ->
            -- Otherwise not allowed (invluding Revisit)
            ( slack, sYield )


withConversation :
    (POV -> Slack)
    -> Convo.Id
    -> POV
    -> Maybe Worque.Work
    -> (Convo -> ( Convo, SubYield ))
    -> ( Slack, SubYield )
withConversation tagger convoId pov work func =
    case Dict.get convoId pov.convos of
        Just convo ->
            let
                ( newConv, sy ) =
                    func convo
            in
            ( tagger { pov | convos = Dict.insert convoId newConv pov.convos }
            , { sy | work = work }
            )

        Nothing ->
            -- Convo somehow gone; should not basically happen
            ( tagger pov, { sYield | persist = True, refreshSources = True, work = work } )


updateFetchStatus : FetchStatus.Msg -> Convo -> ( Convo, SubYield )
updateFetchStatus fMsg convo =
    let
        { fs, persist, triggerRefresh } =
            FetchStatus.update fMsg (Convo.getFetchStatus convo)
    in
    ( Convo.setFetchStatus fs convo
    , { sYield | persist = persist, refreshSources = triggerRefresh }
    )


handleIUnsubscribe : Convo.Id -> Slack -> ( Slack, SubYield )
handleIUnsubscribe convoId slack =
    let
        unsubscribeImpl tagger pov =
            withConversation tagger convoId pov Nothing <|
                updateFetchStatus FetchStatus.Unsub
    in
    case slack of
        Hydrated token pov ->
            unsubscribeImpl (Hydrated token) pov

        Rehydrating token pov ->
            unsubscribeImpl (Rehydrating token) pov

        Expired token pov ->
            unsubscribeImpl (Expired token) pov

        _ ->
            -- Otherwise not allowed, including Revisit
            ( slack, sYield )


handleFetch : Posix -> SlackRegistry -> ( SlackRegistry, Yield )
handleFetch posix sr =
    case readyToFetchTeamAndConv posix sr.dict of
        Just ( teamId, convo ) ->
            withTeam teamId sr <| handleFetchImpl posix convo

        Nothing ->
            ( sr, { yield | work = Just Worque.SlackFetch } )


handleFetchImpl : Posix -> Convo -> Slack -> ( Slack, SubYield )
handleFetchImpl posix convo slack =
    let
        ( newConv, _ ) =
            -- We never persist/refreshSources on Start
            updateFetchStatus (FetchStatus.Start posix) convo
    in
    case slack of
        Hydrated t pov ->
            ( Hydrated t { pov | convos = Dict.insert (Convo.getId convo) newConv pov.convos }
            , { sYield | cmd = fetchMessagesAndBots pov (Team.getId pov.team) convo }
            )

        Rehydrating t pov ->
            ( Rehydrating t { pov | convos = Dict.insert (Convo.getId convo) newConv pov.convos }
            , { sYield | cmd = fetchMessagesAndBots pov (Team.getId pov.team) convo }
            )

        _ ->
            -- Keep next SlackFetch pushed, since it is shared across Teams.
            -- Single Expired Team should not stop the SlackRegistry as a whole.
            ( slack, { sYield | work = Just Worque.SlackFetch } )


readyToFetchTeamAndConv : Posix -> Dict Team.Id Slack -> Maybe ( Team.Id, Convo )
readyToFetchTeamAndConv posix dict =
    -- XXX Looks slowish... could use more optimization?
    let
        reducer teamId slack acc =
            case slack of
                Hydrated _ pov ->
                    recusrivelyFindConvoToFetch posix teamId (Dict.values pov.convos) acc

                Rehydrating _ pov ->
                    recusrivelyFindConvoToFetch posix teamId (Dict.values pov.convos) acc

                _ ->
                    acc
    in
    Dict.foldl reducer Nothing dict


recusrivelyFindConvoToFetch :
    Posix
    -> Team.Id
    -> List Convo
    -> Maybe ( Team.Id, Convo )
    -> Maybe ( Team.Id, Convo )
recusrivelyFindConvoToFetch posix teamId conversations acc =
    case conversations of
        [] ->
            acc

        x :: xs ->
            let
                threshold =
                    case acc of
                        Just ( _, accConv ) ->
                            Convo.getFetchStatus accConv

                        Nothing ->
                            NextFetchAt posix FetchStatus.BO10
            in
            if not (Convo.getIsArchived x) && FetchStatus.lessThan threshold (Convo.getFetchStatus x) then
                recusrivelyFindConvoToFetch posix teamId xs (Just ( teamId, x ))

            else
                recusrivelyFindConvoToFetch posix teamId xs acc


handleIFetched : FetchSuccess -> Slack -> ( Slack, SubYield )
handleIFetched succ slack =
    let
        handleImpl tagger pov =
            let
                newPov =
                    { pov | users = Dict.union succ.users pov.users, bots = Dict.union succ.bots pov.bots }
            in
            withConversation tagger succ.conversationId newPov (Just Worque.SlackFetch) <|
                case succ.messages of
                    [] ->
                        updateFetchStatus (FetchStatus.Miss succ.posix)

                    (m :: _) as ms ->
                        let
                            updateConv ( convo, cy ) =
                                -- Expects messages to be sorted from latest to oldest
                                ( Convo.setLastRead (Just (Message.getTs m)) convo
                                , { cy | persist = True, items = List.reverse ms }
                                )
                        in
                        updateFetchStatus (FetchStatus.Hit succ.posix) >> updateConv
    in
    case slack of
        Hydrated t pov ->
            handleImpl (Hydrated t) pov

        Rehydrating t pov ->
            handleImpl (Rehydrating t) pov

        Expired t pov ->
            handleImpl (Expired t) pov

        _ ->
            -- Should not happen
            ( slack, sYield )


handleITokenInput : String -> Slack -> ( Slack, SubYield )
handleITokenInput token slack =
    case slack of
        Hydrated _ pov ->
            ( Hydrated (Token token) pov, sYield )

        Expired _ pov ->
            ( Expired (Token token) pov, sYield )

        _ ->
            -- Otherwise not allowed
            ( slack, sYield )


handleITokenCommit : Slack -> ( Slack, SubYield )
handleITokenCommit slack =
    case slack of
        Hydrated newToken pov ->
            ( slack, { sYield | cmd = revisit newToken pov } )

        Expired newToken pov ->
            ( slack, { sYield | cmd = revisit newToken pov } )

        _ ->
            -- Otherwise not allowed
            ( slack, sYield )


handleIAPIFailure : Maybe Convo.Id -> RpcFailure -> Slack -> ( Slack, SubYield )
handleIAPIFailure convoIdMaybe f slack =
    case slack of
        Identified { token, team } ->
            -- If successfully Identified, basically Hydrate should not fail. Just retry. TODO limit number of retries
            ( slack, { sYield | cmd = hydrate token (Team.getId team) } )

        Hydrated t pov ->
            -- Failure outside of fetch indicates new token tried but invalid. Just restore previous token.
            updatePovOnIApiFailure (Expired t) (Hydrated pov.token) convoIdMaybe f pov

        Rehydrating t pov ->
            -- Failure outside of fetch indicates rehydration somehow failed. Just restore previous Hydrated state.
            updatePovOnIApiFailure (Expired t) (Hydrated pov.token) convoIdMaybe f pov

        Revisit pov ->
            -- Somehow Revisit failed. Settle with Expired with old pov.
            ( Expired pov.token pov, { sYield | persist = True, refreshSources = True } )

        Expired _ _ ->
            -- Any API failure on Expired. Just keep the state.
            ( slack, sYield )


updatePovOnIApiFailure : (POV -> Slack) -> (POV -> Slack) -> Maybe Convo.Id -> RpcFailure -> POV -> ( Slack, SubYield )
updatePovOnIApiFailure unauthorizedTagger tagger convoIdMaybe f pov =
    case ( f, convoIdMaybe ) of
        -- Slack API basically returns 200 OK, so we consider HttpFailure as transient
        ( HttpFailure _, Just fetchedConvIdStr ) ->
            -- Fail on history fetch
            withConversation tagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                updateFetchStatus FetchStatus.Fail

        ( HttpFailure _, Nothing ) ->
            -- Fail on other
            ( tagger pov, sYield )

        ( RpcError err, Just fetchedConvIdStr ) ->
            if unauthorizedRpc err then
                withConversation unauthorizedTagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                    updateFetchStatus FetchStatus.Fail

            else if conversationUnavailable err then
                let
                    newPov =
                        { pov | convos = Dict.remove fetchedConvIdStr pov.convos }
                in
                ( tagger newPov
                , { sYield | persist = True, work = Just Worque.SlackFetch, refreshSources = True }
                )

            else
                -- Transient otherwise
                withConversation tagger fetchedConvIdStr pov (Just Worque.SlackFetch) <|
                    updateFetchStatus FetchStatus.Fail

        ( RpcError err, Nothing ) ->
            if unauthorizedRpc err then
                ( unauthorizedTagger pov, { sYield | persist = True } )

            else
                -- Transient otherwise
                ( tagger pov, sYield )


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
rpcPostFormTask : Url -> Token -> List ( String, String ) -> Decoder a -> Task RpcFailure a
rpcPostFormTask url token kvPairs dec =
    let
        parts =
            List.map (\( k, v ) -> Http.stringPart k v) (( "token", fromToken token ) :: kvPairs)
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


identify : Token -> Cmd Msg
identify token =
    rpcTry identity UAPIFailure <|
        doT (authTestTask token) <|
            \userId ->
                Task.map2 Identify (Task.map Tuple.second (userInfoTask token userId)) (teamInfoTask token)


{-| Combines identify and hydrate, used on Revisit or Expired. Not requesting to auth.test.

Hydrate is somewhat cheap in Slack compared to Discord, so do it on every reload.
Existing Bot dictionary is kept intact.

-}
revisit : Token -> POV -> Cmd Msg
revisit token pov =
    let
        ( teamId, userId ) =
            ( Team.getId pov.team, User.getId pov.user )
    in
    rpcTry (IRevisit teamId) (IAPIFailure teamId Nothing) <|
        doT (userListTask token) <|
            \users ->
                case Dict.get userId users of
                    Just user ->
                        let
                            updatePov team convos =
                                { pov | user = user, team = team, users = users, convos = convos }
                        in
                        Task.map2 updatePov
                            (teamInfoTask token)
                            (conversationListTask token users)

                    Nothing ->
                        -- Should never happen; requesting user must be included in users
                        Task.fail (RpcError ("Cannot retrieve User: " ++ Id.to userId))


authTestTask : Token -> Task RpcFailure User.Id
authTestTask token =
    rpcPostFormTask (endpoint "/auth.test" Nothing) token [] <|
        D.field "user_id" User.idDecoder


{-| Get a User object for a single user.

To populate User list, use userListTask.
This tasks is specifically for getting current (authenticating) user object,
or supplement user obejcts missing in user list.

-}
userInfoTask : Token -> User.Id -> Task RpcFailure ( User.Id, User )
userInfoTask token userId =
    rpcPostFormTask (endpoint "/users.info" Nothing) token [ ( "user", Id.to userId ) ] <|
        D.map (Tuple.pair userId) (D.field "user" User.decoder)


teamInfoTask : Token -> Task RpcFailure Team
teamInfoTask token =
    rpcPostFormTask (endpoint "/team.info" Nothing) token [] <|
        D.field "team" Team.decoder


hydrate : Token -> Team.Id -> Cmd Msg
hydrate token teamId =
    rpcTry identity (IAPIFailure teamId Nothing) <|
        doT (userListTask token) <|
            \users ->
                Task.map (IHydrate teamId users) (conversationListTask token users)


conversationListTask : Token -> Dict User.Id User -> Task RpcFailure (Dict Convo.Id Convo)
conversationListTask token users =
    rpcPostFormTask (endpoint "/conversations.list" Nothing)
        token
        [ ( "types", "public_channel,private_channel,im,mpim" ) ]
        (D.field "channels" (D.assocListFromList Convo.getId (Convo.decoderForApiResponse users)))


userListTask : Token -> Task RpcFailure (Dict User.Id User)
userListTask token =
    let
        listDecoder =
            D.field "members" (D.list User.decoder)
    in
    -- `users.list` does not have default `limit`
    forwardScrollingPostFormTask (endpoint "/users.list" Nothing) token [] listDecoder Initial
        |> Task.map (List.foldl (\u -> Dict.insert (User.getId u) u) Dict.empty)


fetchMessagesAndBots : POV -> Team.Id -> Convo -> Cmd Msg
fetchMessagesAndBots pov teamId convo =
    let
        convoId =
            Convo.getId convo
    in
    rpcTry (IFetched teamId) (IAPIFailure teamId (Just convoId)) <|
        doT (conversationHistoryTask pov convoId (Convo.getLastRead convo)) <|
            \messages ->
                doT (collectMissingInfoTask pov.token messages) <|
                    \( users, bots ) ->
                        let
                            finalize =
                                FetchSuccess convoId (List.map (Message.fillAuthor users bots) messages) users bots
                        in
                        Task.map finalize Time.now


type CursorIn a
    = Initial
    | CursorIn String (List a)


type CursorOut a
    = CursorOut String (List a)
    | Done (List a)


forwardScrollingPostFormTask : Url -> Token -> List ( String, String ) -> Decoder (List a) -> CursorIn a -> Task RpcFailure (List a)
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


conversationHistoryTask : POV -> Convo.Id -> Maybe Ts -> Task RpcFailure (List Message)
conversationHistoryTask pov convoId lrMaybe =
    let
        baseParams =
            -- Recommended to be no more than 200; https://api.slack.com/methods/conversations.history
            [ ( "channel", Id.to convoId ), ( "limit", "200" ) ]

        baseDecoder =
            D.field "messages" (D.leakyList (Message.decoderForApiResponse pov.users pov.bots pov.convos convoId))

        url =
            endpoint "/conversations.history" Nothing
    in
    case lrMaybe of
        Just ts ->
            forwardScrollingPostFormTask url pov.token (( "oldest", Ts.toString ts ) :: baseParams) baseDecoder Initial

        Nothing ->
            -- Means never fetched. Without `latest`, it fetches up to current time,
            -- and the first segment returned IS the latest one. (No need to scrolling backward)
            rpcPostFormTask url pov.token baseParams baseDecoder


{-| Get a Bot object for a single Bot.

Bot API does not have "list" method.

-}
botInfoTask : Token -> Bot.Id -> Task RpcFailure ( Bot.Id, Bot )
botInfoTask token botId =
    rpcPostFormTask (endpoint "/bots.info" Nothing) token [ ( "bot", Id.to botId ) ] <|
        D.map (Tuple.pair botId) (D.field "bot" Bot.decoder)


collectMissingInfoTask : Token -> List Message -> Task RpcFailure ( Dict User.Id User, Dict Bot.Id Bot )
collectMissingInfoTask token messages =
    let
        ( userIds, botIds ) =
            Message.collectMissingAuthorIds messages
    in
    Task.map2 (\users bots -> ( Dict.fromList users, Dict.fromList bots ))
        (Task.sequence (List.map (userInfoTask token) userIds))
        (Task.sequence (List.map (botInfoTask token) botIds))



-- Runtime APIs


getConvoFromCache : Convo.Id -> List ConvoCache -> Maybe ConvoCache
getConvoFromCache convoId caches =
    List.Extra.find (\c -> c.id == convoId) caches


defaultIconUrl : Maybe Int -> String
defaultIconUrl sizeMaybe =
    logoCdnUrl sizeMaybe "/pl546j-7le8zk-199wkt/Slack%20Mark.png"


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
