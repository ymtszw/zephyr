module Data.Producer.Slack.ConvoCache exposing (ConvoCache, decoder, encode)

import AssocList exposing (Dict)
import Data.Producer.Slack.Convo as Convo
import Data.Producer.Slack.Team as Team exposing (Team)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E


{-| Rarely updated prts of Convo. Saved in ColumnStore for fast and atomic reference.

Adding Team info since ConversationCache can be mixed up with ones from other Teams in FAM.

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
