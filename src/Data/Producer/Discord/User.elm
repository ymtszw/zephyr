module Data.Producer.Discord.User exposing
    ( User, Id, encode, decoder, new
    , getId, getUsername, getDiscriminator
    , avatarUrl
    )

{-| User object.

@docs User, Id, encode, decoder, new
@docs getId, getUsername, getDiscriminator
@docs avatarUrl

-}

import Data.Producer.Discord.Cdn exposing (makeDefaultIconUrl, makeUrl)
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
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


new : User
new =
    User
        { id = Id.from "id"
        , username = "username"
        , discriminator = "discriminator"
        , avatar = Nothing
        }


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
    D.succeed new
        |> D.map2 setId (D.field "id" (Id.decoder D.string))
        |> D.map2 setUsername (D.field "username" D.string)
        |> D.map2 setDiscriminator (D.field "discriminator" D.string)
        |> D.map2 setAvatar (D.field "avatar" (D.maybe (D.map AvatarHash D.string)))


avatarUrl : Maybe Int -> User -> String
avatarUrl sizeMaybe (User u) =
    case u.avatar of
        Just (AvatarHash hash) ->
            makeUrl sizeMaybe ("/avatars/" ++ Id.to u.id ++ "/" ++ hash ++ ".png")

        Nothing ->
            makeDefaultIconUrl sizeMaybe u.discriminator



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


setId : Id -> User -> User
setId val (User u) =
    User { u | id = val }


setUsername : String -> User -> User
setUsername val (User u) =
    User { u | username = val }


setDiscriminator : String -> User -> User
setDiscriminator val (User u) =
    User { u | discriminator = val }


setAvatar : Maybe AvatarHash -> User -> User
setAvatar val (User u) =
    User { u | avatar = val }
