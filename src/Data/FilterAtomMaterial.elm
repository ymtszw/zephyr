module Data.FilterAtomMaterial exposing
    ( FilterAtomMaterial, init, encode, decoder
    , UpdateInstruction(..), update, mapDiscordChannel
    )

{-| Cache of relatively long-living information used for rendering FilterAtom.

Although a cache, it is persisted to IndexedDB, in order to display column information
at application load, without waiting for loading ProducerRegistry.

@docs FilterAtomMaterial, init, encode, decoder
@docs UpdateInstruction, update, mapDiscordChannel

-}

import Data.Filter as Filter exposing (FilterAtom)
import Data.Producer exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import ListExtra


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, List Discord.ChannelCache ) -- List instead of Dict, should be sorted already
    , ofSlackConversation : Maybe ( FilterAtom, List Slack.ConversationCache )
    }


init : FilterAtomMaterial
init =
    { ofDiscordChannel = Nothing
    , ofSlackConversation = Nothing
    }


encode : FilterAtomMaterial -> E.Value
encode fam =
    E.object
        [ ( "ofDiscordChannel", E.maybe encodeDiscordMaterial fam.ofDiscordChannel )
        , ( "ofSlackConversation", E.maybe encodeSlackMaterial fam.ofSlackConversation )
        ]


encodeDiscordMaterial : ( FilterAtom, List Discord.ChannelCache ) -> E.Value
encodeDiscordMaterial ( defaultAtom, channelsCache ) =
    let
        ( encodedChannels, guilds ) =
            prepareDiscordValues channelsCache
    in
    E.object
        [ ( "default", Filter.encodeFilterAtom defaultAtom )
        , ( "guilds", E.dict identity Discord.encodeGuild guilds )
        , ( "channels", E.list identity encodedChannels )
        ]


prepareDiscordValues : List Discord.ChannelCache -> ( List E.Value, Dict String Discord.Guild )
prepareDiscordValues channelCache =
    let
        encodeChannelAndCollectGuild c ( acc, guilds ) =
            Tuple.pair (Discord.encodeChannelCache c :: acc) <|
                case c.guildMaybe of
                    Just g ->
                        Dict.insert g.id g guilds

                    Nothing ->
                        guilds
    in
    -- Notice the order; channelCache is already sorted, must keep it
    List.foldr encodeChannelAndCollectGuild ( [], Dict.empty ) channelCache


encodeSlackMaterial : ( FilterAtom, List Slack.ConversationCache ) -> E.Value
encodeSlackMaterial ( defaultAtom, convsCache ) =
    E.object
        [ ( "default", Filter.encodeFilterAtom defaultAtom )
        , ( "conversations", E.list Slack.encodeConversationCache convsCache )
        ]


decoder : Decoder FilterAtomMaterial
decoder =
    D.map2 FilterAtomMaterial
        (D.field "ofDiscordChannel" (D.maybe discordMaterialDecoder))
        (D.field "ofSlackConversation" (D.maybe slackMaterialDecoder))


discordMaterialDecoder : Decoder ( FilterAtom, List Discord.ChannelCache )
discordMaterialDecoder =
    D.do (D.field "default" Filter.filterAtomDecoder) <|
        \fa ->
            D.do (D.field "guilds" (D.dict Discord.guildDecoder)) <|
                \guilds ->
                    D.do (D.field "channels" (D.list (Discord.channelCacheDecoder guilds))) <|
                        \channelCache ->
                            D.succeed ( fa, channelCache )


slackMaterialDecoder : Decoder ( FilterAtom, List Slack.ConversationCache )
slackMaterialDecoder =
    D.map2 Tuple.pair
        (D.field "default" Filter.filterAtomDecoder)
        (D.field "conversations" (D.list Slack.conversationCacheDecoder))



-- Update


type UpdateInstruction
    = DiscordInstruction (UpdateFAM ( FilterAtom, List Discord.ChannelCache ))
    | SlackInstruction (UpdateFAM ())


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

        (SlackInstruction _) :: xs ->
            -- TODO
            updateImpl xs ( fam, persist )


mapDiscordChannel : String -> FilterAtomMaterial -> (Discord.ChannelCache -> a) -> Maybe a
mapDiscordChannel cId fam mapper =
    fam.ofDiscordChannel
        |> Maybe.andThen (\( _, channels ) -> ListExtra.findOne (.id >> (==) cId) channels)
        |> Maybe.map mapper
