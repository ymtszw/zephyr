module Data.FilterAtomMaterial exposing
    ( FilterAtomMaterial, init, encode, decoder
    , UpdateInstruction(..), update
    , findDiscordChannel, findSlackConvoCache
    )

{-| Cache of relatively long-living information used for rendering FilterAtom.

Although a cache, it is persisted to IndexedDB, in order to display column information
at application load, without waiting for loading ProducerRegistry.

@docs FilterAtomMaterial, init, encode, decoder
@docs UpdateInstruction, update
@docs findDiscordChannel, findSlackConvoCache

-}

import Data.Producer exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord
import Data.Producer.Discord.Channel as Channel
import Data.Producer.Discord.ChannelCache exposing (ChannelCache)
import Data.Producer.Slack as Slack
import Data.Producer.Slack.Convo as Convo
import Data.Producer.Slack.ConvoCache exposing (ConvoCache)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Json.EncodeExtra as E
import List.Extra


{-| TODO Refactor data structure with Source and Filter in mind; <https://github.com/ymtszw/zephyr/issues/64>
-}
type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe Discord.FAM
    , ofSlackConversation : Maybe Slack.FAM
    }


init : FilterAtomMaterial
init =
    { ofDiscordChannel = Nothing
    , ofSlackConversation = Nothing
    }


encode : FilterAtomMaterial -> E.Value
encode fam =
    E.object
        [ ( "ofDiscordChannel", E.maybe Discord.encodeFam fam.ofDiscordChannel )
        , ( "ofSlackConversation", E.maybe Slack.encodeFam fam.ofSlackConversation )
        ]


decoder : Decoder FilterAtomMaterial
decoder =
    D.map2 FilterAtomMaterial
        (D.field "ofDiscordChannel" (D.maybe Discord.famDecoder))
        (D.field "ofSlackConversation" (D.maybe Slack.famDecoder))



-- Update


type UpdateInstruction
    = DiscordInstruction (UpdateFAM Discord.FAM)
    | SlackInstruction (UpdateFAM Slack.FAM)


update : List UpdateInstruction -> FilterAtomMaterial -> ( FilterAtomMaterial, Bool )
update instructions fam =
    updateImpl instructions ( fam, False )


updateImpl : List UpdateInstruction -> ( FilterAtomMaterial, Bool ) -> ( FilterAtomMaterial, Bool )
updateImpl instructions ( fam, persist ) =
    case instructions of
        [] ->
            ( fam, persist )

        (DiscordInstruction (SetFAM discordFAM)) :: xs ->
            updateImpl xs ( { fam | ofDiscordChannel = Just discordFAM }, True )

        (DiscordInstruction KeepFAM) :: xs ->
            updateImpl xs ( fam, persist )

        (DiscordInstruction DestroyFAM) :: xs ->
            updateImpl xs ( { fam | ofDiscordChannel = Nothing }, True )

        (SlackInstruction (SetFAM slackFAM)) :: xs ->
            updateImpl xs ( { fam | ofSlackConversation = Just slackFAM }, True )

        (SlackInstruction KeepFAM) :: xs ->
            updateImpl xs ( fam, persist )

        (SlackInstruction DestroyFAM) :: xs ->
            updateImpl xs ( { fam | ofSlackConversation = Nothing }, True )



-- APIs


findDiscordChannel : Channel.Id -> FilterAtomMaterial -> Maybe ChannelCache
findDiscordChannel cId fam =
    Maybe.andThen (\( _, caches ) -> List.Extra.find (\c -> c.id == cId) caches) fam.ofDiscordChannel


findSlackConvoCache : Convo.Id -> FilterAtomMaterial -> Maybe ConvoCache
findSlackConvoCache cId fam =
    Maybe.andThen (\sFam -> Slack.getConvoFromCache cId sFam.convos) fam.ofSlackConversation
