module Data.Producer.Slack.Convo exposing
    ( Convo, Id, Type(..), convo
    , encode, encodeType
    , decoder, idDecoder, typeDecoder, decoderForApiResponse
    , getId, getName, getIsArchived, getLastRead, getType_, getFetchStatus
    , setId, setName, setIsArchived, setLastRead, setType_, setFetchStatus
    , resolveConvoName, compare, compareByMembersipThenName
    , isChannel, isPrivate
    )

{-| Channel-like objects.

@docs Convo, Id, Type, convo
@docs encode, encodeType
@docs decoder, idDecoder, typeDecoder, decoderForApiResponse
@docs getId, getName, getIsArchived, getLastRead, getType_, getFetchStatus
@docs setId, setName, setIsArchived, setLastRead, setType_, setFetchStatus
@docs resolveConvoName, compare, compareByMembersipThenName
@docs isChannel, isPrivate

-}

import AssocList exposing (Dict)
import Data.Producer.FetchStatus as FetchStatus exposing (FetchStatus(..))
import Data.Producer.Slack.Ts as Ts exposing (Ts)
import Data.Producer.Slack.User as User exposing (User)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


{-| Channel-like objects. Includes public/private channels and IM/MPIMs.

<https://api.slack.com/types/conversation>
<https://api.slack.com/methods/conversations.list>

Shortening as "Conversation" is long.

For IMs, we need the other member's User object for its name.

For MPIMs, we should consider how `name`s are shown,
since `name` values of MPIMs are auto-generated strings.

Members of private conversations must be retrieved from conversations.members API.
<https://api.slack.com/methods/conversations.members>
(Although conversation doc says there is `members` field, it is retired.
<https://api.slack.com/changelog/2017-10-members-array-truncating>)

We do not use `last_read` returned from APIs directly,
rather we locally record timestamps of actually retrieved messages.

This object COULD include Team, but deliberately excludng it for easier testing.

TODO: consider how to support IM/MPIMs, while aligning with Discord DM/GroupDM

-}
type Convo
    = Convo ConvoRecord


type alias ConvoRecord =
    { id : Id
    , name : String -- May start with values from API response, but should cannonicalize on Hydrate/Rehydrate
    , isArchived : Bool
    , lastRead : Maybe Ts -- For IM, `last_read` is not supplied from conversation object. Though we do not directly use them anyway.

    -- Zephyr local
    , type_ : Type
    , fetchStatus : FetchStatus
    }


{-| Smart constructor for tests.
-}
convo : Id -> String -> Bool -> Maybe Ts -> Type -> FetchStatus -> Convo
convo id name isArchived lastRead type_ fetchStatus =
    Convo { id = id, name = name, isArchived = isArchived, lastRead = lastRead, type_ = type_, fetchStatus = fetchStatus }


type alias Id =
    Id.Id String Convo


type Type
    = PublicChannel IsMember
    | PrivateChannel
    | IM -- Instant Messages, presumably
    | MPIM -- Multi-person IM


type alias IsMember =
    Bool



-- Codec


encode : Convo -> E.Value
encode (Convo c) =
    E.object
        [ ( "id", Id.encode E.string c.id )
        , ( "name", E.string c.name )
        , ( "is_archived", E.bool c.isArchived )
        , ( "last_read", E.maybe Ts.encode c.lastRead )
        , ( "type_", encodeType c.type_ )
        , ( "fetchStatus", FetchStatus.encode c.fetchStatus )
        ]


encodeType : Type -> E.Value
encodeType type_ =
    case type_ of
        PublicChannel isMember ->
            E.tagged "PublicChannel" (E.bool isMember)

        PrivateChannel ->
            E.tag "PrivateChannel"

        IM ->
            E.tag "IM"

        MPIM ->
            E.tag "MPIM"


decoder : Decoder Convo
decoder =
    D.map Convo <|
        D.map6 ConvoRecord
            (D.field "id" idDecoder)
            (D.field "name" D.string)
            (D.optionField "is_archived" D.bool False)
            (D.maybeField "last_read" lastReadDecoder)
            (D.field "type_" typeDecoder)
            (D.optionField "fetchStatus" FetchStatus.decoder Available)


idDecoder : Decoder Id
idDecoder =
    D.oneOf
        [ Id.decoder D.string
        , -- Old format
          D.tagged "ConversationId" Id.from D.string
        ]


lastReadDecoder : Decoder Ts
lastReadDecoder =
    -- As in Discord's lastMessageId, we deliberately ignore "last_read" from Slack API.
    -- So this decoder MUST NOT succeed against bare values in API responses.
    D.oneOf
        [ Ts.decoder
        , -- Old format
          D.tagged "LastRead" identity Ts.decoder
        ]


typeDecoder : Decoder Type
typeDecoder =
    D.oneOf
        [ D.tagged "PublicChannel" PublicChannel D.bool
        , D.tag "PrivateChannel" PrivateChannel
        , D.tag "IM" IM
        , D.tag "MPIM" MPIM

        -- Old formats where Type was only used in cache
        , D.tag "PublicChannelCache" (PublicChannel True) -- Making up True for new type, but IsMember must be properly rehydrated
        , D.tag "PrivateChannelCache" PrivateChannel
        , D.tag "IMCache" IM
        , D.tag "MPIMCache" MPIM
        ]


decoderForApiResponse : Dict User.Id User -> Decoder Convo
decoderForApiResponse users =
    let
        typeDecoderForApiResponse =
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
                    D.field "user" User.idDecoder |> D.map (User.resolveUserName users)

                MPIM ->
                    -- TODO cannonicalize name; need to request to `conversations.members`?
                    D.field "name" D.string

                _ ->
                    D.field "name" D.string

        isArchivedDecoder type_ =
            case type_ of
                IM ->
                    D.field "is_user_deleted" D.bool

                MPIM ->
                    D.succeed False

                _ ->
                    D.field "is_archived" D.bool
    in
    D.do typeDecoderForApiResponse <|
        \type_ ->
            D.do (resolveName type_) <|
                \name ->
                    D.do (isArchivedDecoder type_) <|
                        \isArchived ->
                            D.do (D.field "id" idDecoder) <|
                                \id ->
                                    D.map Convo <|
                                        D.succeed <|
                                            ConvoRecord id name isArchived Nothing type_ Available



-- Runtime APIs


resolveConvoName : Dict Id Convo -> Id -> String
resolveConvoName convos id =
    AssocList.get id convos
        |> Maybe.map (\(Convo c) -> c.name)
        |> Maybe.withDefault (Id.to id)


compare : Convo -> Convo -> Order
compare (Convo c1) (Convo c2) =
    compareByMembersipThenName c1 c2


{-| Can also work against ConvoCaches.
-}
compareByMembersipThenName : { x | name : String, type_ : Type } -> { x | name : String, type_ : Type } -> Order
compareByMembersipThenName c1 c2 =
    if c1 == c2 then
        EQ

    else
        let
            compareToPub isMember =
                if isMember then
                    GT

                else
                    LT
        in
        case ( c1.type_, c2.type_ ) of
            ( PublicChannel isMemberA, PublicChannel isMemberB ) ->
                case ( isMemberA, isMemberB ) of
                    ( True, False ) ->
                        LT

                    ( False, True ) ->
                        GT

                    _ ->
                        Basics.compare c1.name c2.name

            ( PublicChannel True, _ ) ->
                LT

            ( PublicChannel False, _ ) ->
                GT

            ( PrivateChannel, PublicChannel isMember ) ->
                compareToPub isMember

            ( PrivateChannel, PrivateChannel ) ->
                Basics.compare c1.name c2.name

            ( PrivateChannel, _ ) ->
                LT

            ( IM, PublicChannel isMember ) ->
                compareToPub isMember

            ( IM, IM ) ->
                Basics.compare c1.name c2.name

            ( IM, MPIM ) ->
                LT

            ( IM, _ ) ->
                GT

            ( MPIM, PublicChannel isMember ) ->
                compareToPub isMember

            ( MPIM, MPIM ) ->
                Basics.compare c1.name c2.name

            ( MPIM, _ ) ->
                GT


isChannel : Type -> Bool
isChannel type_ =
    case type_ of
        PublicChannel _ ->
            True

        PrivateChannel ->
            True

        IM ->
            False

        MPIM ->
            False


isPrivate : Type -> Bool
isPrivate type_ =
    case type_ of
        PublicChannel _ ->
            False

        PrivateChannel ->
            True

        IM ->
            True

        MPIM ->
            True



-- Accessors


getId : Convo -> Id
getId (Convo c) =
    c.id


getName : Convo -> String
getName (Convo c) =
    c.name


getIsArchived : Convo -> Bool
getIsArchived (Convo c) =
    c.isArchived


getLastRead : Convo -> Maybe Ts
getLastRead (Convo c) =
    c.lastRead


getType_ : Convo -> Type
getType_ (Convo c) =
    c.type_


getFetchStatus : Convo -> FetchStatus
getFetchStatus (Convo c) =
    c.fetchStatus


setId : Id -> Convo -> Convo
setId val (Convo c) =
    Convo { c | id = val }


setName : String -> Convo -> Convo
setName val (Convo c) =
    Convo { c | name = val }


setIsArchived : Bool -> Convo -> Convo
setIsArchived val (Convo c) =
    Convo { c | isArchived = val }


setLastRead : Maybe Ts -> Convo -> Convo
setLastRead val (Convo c) =
    Convo { c | lastRead = val }


setType_ : Type -> Convo -> Convo
setType_ val (Convo c) =
    Convo { c | type_ = val }


setFetchStatus : FetchStatus -> Convo -> Convo
setFetchStatus val (Convo c) =
    Convo { c | fetchStatus = val }
