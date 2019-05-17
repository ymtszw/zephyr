module Data.Producer.Slack.ConvoCache exposing
    ( ConvoCache, from, encode, decoder, compare
    , encodeList, listDecoder
    )

{-| Cache of Slack.Channel. Stored in ColumnStore.

@docs ConvoCache, from, encode, decoder, compare
@docs encodeList, listDecoder

-}

import AssocList exposing (Dict)
import Data.Producer.Slack.Convo as Convo exposing (Convo)
import Data.Producer.Slack.Team as Team exposing (Team)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E


{-| Rarely updated prts of Convo. Saved in ColumnStore for fast and atomic referencing.

Adding Team info since ConvoCache can be mixed up with ones from other Teams in FAM.

Also it is a bare Record, since it exists mainly for View usage.

-}
type alias ConvoCache =
    { id : Convo.Id
    , name : String
    , isArchived : Bool
    , type_ : Convo.Type
    , team : Team -- Unjoined on save
    }


encode : ConvoCache -> E.Value
encode cache =
    E.object
        [ ( "id", Id.encode E.string cache.id )
        , ( "name", E.string cache.name )
        , ( "is_archived", E.bool cache.isArchived )
        , ( "type_", Convo.encodeType cache.type_ )
        , ( "team_id", Id.encode E.string (Team.getId cache.team) )
        ]


encodeList : List ConvoCache -> E.Value
encodeList convos =
    let
        ( encodedConvos, teams ) =
            unjoinTeamsAndEncodeConvos convos
    in
    E.object
        [ ( "convos", E.list identity encodedConvos )
        , ( "teams", E.assocList Id.to Team.encode teams )
        ]


unjoinTeamsAndEncodeConvos : List ConvoCache -> ( List E.Value, Dict Team.Id Team )
unjoinTeamsAndEncodeConvos convos =
    let
        reducer c ( accList, accTeams ) =
            ( encode c :: accList, AssocList.insert (Team.getId c.team) c.team accTeams )
    in
    -- Conserve the order of convos; already sorted
    List.foldr reducer ( [], AssocList.empty ) convos


decoder : Dict Team.Id Team -> Decoder ConvoCache
decoder teams =
    D.do (D.field "team_id" Team.idDecoder) <|
        \teamId ->
            case AssocList.get teamId teams of
                Just team ->
                    D.map5 ConvoCache
                        (D.field "id" Convo.idDecoder)
                        (D.field "name" D.string)
                        (D.optionField "is_archived" D.bool False)
                        (D.field "type_" Convo.typeDecoder)
                        (D.succeed team)

                Nothing ->
                    -- Should not happen
                    D.fail ("Team [" ++ Id.to teamId ++ "] is not cached!")


listDecoder : Decoder (List ConvoCache)
listDecoder =
    D.do (D.field "teams" (D.assocList Id.from Team.decoder)) <|
        \teams ->
            D.field "convos" (D.list (decoder teams))


from : Team -> Convo -> ConvoCache
from team c =
    { id = Convo.getId c
    , name = Convo.getName c
    , isArchived = Convo.getIsArchived c
    , type_ = Convo.getType_ c
    , team = team
    }


compare : ConvoCache -> ConvoCache -> Order
compare cache1 cache2 =
    case Basics.compare (Team.getName cache1.team) (Team.getName cache2.team) of
        EQ ->
            Convo.compareByMembersipThenName cache1 cache2

        diff ->
            diff
