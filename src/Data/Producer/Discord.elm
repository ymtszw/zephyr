module Data.Producer.Discord exposing (Discord, decoder, encode, endpoint, receive)

{-| Realtime Producer for Discord.

Using Discord's realtime communication backend: "Gateway".

<https://discordapp.com/developers/docs/topics/gateway>

Note that it involves a little "shady" work on retrieving
full-privilege personal token for a Discord user. Discuss in private.

-}

import Data.Item exposing (Item)
import Data.Producer.Realtime as Realtime exposing (Reply(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Websocket exposing (Endpoint(..))


type alias Discord =
    { tag : Tag
    , token : String
    }


type Tag
    = DiscordTag


decoder : Decoder Discord
decoder =
    D.map2 Discord
        (D.field "tag" (D.string |> D.andThen tagDecoder))
        (D.field "token" D.string)


tagDecoder : String -> Decoder Tag
tagDecoder rawTag =
    case rawTag of
        "discord" ->
            D.succeed DiscordTag

        _ ->
            D.fail "Not a 'discord' tag."


encode : Discord -> E.Value
encode { token } =
    E.object
        [ ( "tag", E.string "discord" )
        , ( "token", E.string token )
        ]



-- INTERFACES


endpoint : Endpoint
endpoint =
    Endpoint "wss://gateway.discord.gg/?v=6&encoding=json"


receive : Realtime.Handler Discord
receive discord message =
    ( [ Data.Item.textOnly message ], discord, NoReply )
