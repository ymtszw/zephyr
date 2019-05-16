module Data.Column.Source exposing (Source(..), encode, decoder)

{-| Source of items consumed by a column.

Data.Items that matches Source conditions will be added to the column.

@docs Source, encode, decoder

-}

import Data.Producer.Discord.Channel as DiscordChannel
import Data.Producer.Slack.Convo as SlackConvo
import Data.Producer.Slack.Team as SlackTeam
import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E exposing (Value)
import Json.EncodeExtra as E


{-| Identification data for data sources.
-}
type Source
    = DiscordChannel DiscordChannel.Id
    | SlackConvo SlackTeam.Id SlackConvo.Id


encode : Source -> Value
encode s =
    case s of
        DiscordChannel id ->
            E.tagged "DiscordChannel" (Id.encode E.string id)

        SlackConvo teamId convoId ->
            E.tagged2 "SlackConvo" (Id.encode E.string teamId) (Id.encode E.string convoId)


decoder : Decoder Source
decoder =
    D.oneOf
        [ D.tagged "DiscordChannel" DiscordChannel (Id.decoder D.string)
        , D.tagged2 "SlackConvo" SlackConvo (Id.decoder D.string) (Id.decoder D.string)
        ]
