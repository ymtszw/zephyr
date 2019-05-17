module Data.FilterAtomMaterial exposing (FilterAtomMaterial, decoder)

{-| XXX **DEPRECATED** Cache of relatively long-living information used for rendering FilterAtom.

Remove after migration to AvailableSources.

@docs FilterAtomMaterial, decoder

-}

import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Json.Decode as D exposing (Decoder)


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe Discord.FAM
    , ofSlackConversation : Maybe Slack.FAM
    }


decoder : Decoder FilterAtomMaterial
decoder =
    D.map2 FilterAtomMaterial
        (D.field "ofDiscordChannel" (D.maybe Discord.famDecoder))
        (D.field "ofSlackConversation" (D.maybe Slack.famDecoder))
