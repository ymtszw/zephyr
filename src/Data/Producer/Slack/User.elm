module Data.Producer.Slack.User exposing
    ( User, Id, UserProfile, encode, decoder, idDecoder
    , getId, getTeamId, getProfile
    , resolveUserName
    )

{-| A user object.

<https://api.slack.com/types/user>
<https://api.slack.com/methods/users.info>

@docs User, Id, UserProfile, encode, decoder, idDecoder
@docs getId, getTeamId, getProfile
@docs resolveUserName

-}

import AssocList exposing (Dict)
import Data.Producer.Slack.Team as Team
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url exposing (Url)


{-| A user object.

<https://api.slack.com/types/user>
<https://api.slack.com/methods/users.info>

-}
type User
    = User UserRecord


type alias UserRecord =
    { id : Id
    , teamId : Team.Id
    , profile : UserProfile
    }


type alias Id =
    Id.Id String User


type alias UserProfile =
    { displayName : Maybe String -- Can be empty string!! Treat empty string as Nothing
    , realName : String
    , statusText : String
    , statusEmoji : String
    , image32 : Url
    , image48 : Url
    }


encode : User -> E.Value
encode (User user) =
    E.object
        [ ( "id", Id.encode E.string user.id )
        , ( "team_id", Id.encode E.string user.teamId )
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


decoder : Decoder User
decoder =
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
    D.map User <|
        D.map3 UserRecord
            (D.field "id" idDecoder)
            (D.field "team_id" Team.idDecoder)
            (D.field "profile" profileDecoder)


idDecoder : Decoder Id
idDecoder =
    D.oneOf
        [ Id.decoder D.string
        , -- Old format
          D.tagged "UserId" Id.from D.string
        ]



-- Runtime APIs


getId : User -> Id
getId (User user) =
    user.id


getTeamId : User -> Team.Id
getTeamId (User user) =
    user.teamId


getProfile : User -> UserProfile
getProfile (User user) =
    user.profile


resolveUserName : Dict Id User -> Id -> String
resolveUserName users id =
    AssocList.get id users
        |> Maybe.map (\(User u) -> Maybe.withDefault u.profile.realName u.profile.displayName)
        |> Maybe.withDefault (Id.to id)
