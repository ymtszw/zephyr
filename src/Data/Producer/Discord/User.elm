module Data.Producer.Discord.User exposing
    ( User, Id, encode, decoder
    , getId, getUsername, getDiscriminator
    , avatarUrl
    )

{-| User object.

@docs User, Id, encode, decoder
@docs getId, getUsername, getDiscriminator
@docs avatarUrl

-}

import Data.Producer.Discord.Cdn exposing (makeUrl)
import Id
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Json.EncodeExtra as E


{-| User object.

Note that there is also Guild Member objects, which contains
Guild-local information of Users such as nicknames.
This might be introduced later.

<https://discordapp.com/developers/docs/resources/user#user-object>

-}
type User
    = User UserRecord


type alias UserRecord =
    { id : Id
    , username : String
    , discriminator : String
    , avatar : Maybe AvatarHash
    }


type alias Id =
    Id.Id String User


type AvatarHash
    = AvatarHash String


fromAvatarHash : AvatarHash -> String
fromAvatarHash (AvatarHash hash) =
    hash


encode : User -> E.Value
encode (User user) =
    E.object
        [ ( "id", Id.encode E.string user.id )
        , ( "username", E.string user.username )
        , ( "discriminator", E.string user.discriminator )
        , ( "avatar", E.maybe (E.string << fromAvatarHash) user.avatar )
        ]


decoder : Decoder User
decoder =
    D.map User <|
        D.map4 UserRecord
            (D.field "id" (Id.decoder D.string))
            (D.field "username" D.string)
            (D.field "discriminator" D.string)
            (D.field "avatar" (D.maybe (D.map AvatarHash D.string)))


avatarUrl : Maybe Int -> User -> String
avatarUrl sizeMaybe (User u) =
    makeUrl sizeMaybe <|
        case u.avatar of
            Just (AvatarHash hash) ->
                "/avatars/" ++ Id.to u.id ++ "/" ++ hash ++ ".png"

            Nothing ->
                case String.toInt u.discriminator of
                    Just int ->
                        "/embed/avatars/" ++ String.fromInt (modBy 5 int) ++ ".png"

                    Nothing ->
                        "/embed/avatars/0.png"



-- Runtime APIs


getId : User -> Id
getId (User u) =
    u.id


getUsername : User -> String
getUsername (User u) =
    u.username


getDiscriminator : User -> String
getDiscriminator (User u) =
    u.discriminator
