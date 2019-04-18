module Data.Producer.Discord.Guild exposing (Guild, Id, decoder, encode, getId, getName, iconUrl)

import Data.Producer.Discord.Cdn exposing (makeUrl)
import Id
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Json.EncodeExtra as E


{-| Guild object.

<https://discordapp.com/developers/docs/resources/guild#guild-object>

-}
type Guild
    = Guild GuildRecord


type alias GuildRecord =
    { id : Id
    , name : String
    , icon : Maybe IconHash
    }


type alias Id =
    Id.Id String Guild


type IconHash
    = IconHash String


fromIconHash : IconHash -> String
fromIconHash (IconHash hash) =
    hash


encode : Guild -> E.Value
encode (Guild guild) =
    E.object
        [ ( "id", Id.encode E.string guild.id )
        , ( "name", E.string guild.name )
        , ( "icon", E.maybe (E.string << fromIconHash) guild.icon )
        ]


decoder : Decoder Guild
decoder =
    D.map Guild <|
        D.map3 GuildRecord
            (D.field "id" (Id.decoder D.string))
            (D.field "name" D.string)
            (D.field "icon" (D.maybe (D.map IconHash D.string)))



-- Runtime APIs


iconUrl : Maybe Int -> Guild -> Maybe String
iconUrl sizeMaybe (Guild g) =
    -- If icon is not set, we use Icon.abbr rather than fallback icon (Discord logo).
    case g.icon of
        Just (IconHash hash) ->
            Just (makeUrl sizeMaybe ("/icons/" ++ Id.to g.id ++ "/" ++ hash ++ ".png"))

        Nothing ->
            Nothing


getId : Guild -> Id
getId (Guild g) =
    g.id


getName : Guild -> String
getName (Guild g) =
    g.name
