module Data.Producer.Slack.Bot exposing
    ( Bot, Id, encode, decoder, idDecoder
    , getId, getName, getIcons
    )

{-| Bot object.

@docs Bot, Id, BotIcons, encode, decoder, idDecoder
@docs getId, getName, getIcons

-}

import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url exposing (Url)


{-| Bot entity in the workspace.

A Bot is somewhat User-like, but different in many ways.
<https://api.slack.com/methods/bots.info>

We cannot enumerate available bots ahead of time by API (`bots.list` does not exist),
so we must lazily acquire/update Bots' info when we see `bot_id` in messages, and store them in local dictionary.

-}
type Bot
    = Bot BotRecord


type alias BotRecord =
    { id : Id
    , name : String
    , icons : BotIcons -- Yeah, plural
    }


type alias BotIcons =
    { image36 : Url
    , image48 : Url
    , image72 : Url
    }


type alias Id =
    Id.Id String Bot


encode : Bot -> E.Value
encode (Bot bot) =
    E.object
        [ ( "id", Id.encode E.string bot.id )
        , ( "name", E.string bot.name )
        , Tuple.pair "icons" <|
            E.object
                [ ( "image_36", E.url bot.icons.image36 )
                , ( "image_48", E.url bot.icons.image48 )
                , ( "image_72", E.url bot.icons.image72 )
                ]
        ]


decoder : Decoder Bot
decoder =
    let
        iconsDecoder =
            D.map3 BotIcons
                (D.field "image_36" D.url)
                (D.field "image_48" D.url)
                (D.field "image_72" D.url)
    in
    D.map Bot <|
        D.map3 BotRecord
            (D.field "id" idDecoder)
            (D.field "name" D.string)
            (D.field "icons" iconsDecoder)


idDecoder : Decoder Id
idDecoder =
    D.oneOf
        [ Id.decoder D.string
        , --Old format
          D.tagged "BotId" Id.from D.string
        ]


getId : Bot -> Id
getId (Bot bot) =
    bot.id


getName : Bot -> String
getName (Bot bot) =
    bot.name


getIcons : Bot -> BotIcons
getIcons (Bot bot) =
    bot.icons
