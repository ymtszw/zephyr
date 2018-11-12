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
import Data.Producer.Base exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import ListExtra


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, List Discord.ChannelCache ) -- List instead of Dict, should be sorted already
    }


init : FilterAtomMaterial
init =
    { ofDiscordChannel = Nothing }


encode : FilterAtomMaterial -> E.Value
encode fam =
    E.object
        [ Tuple.pair "ofDiscordChannel" <|
            case fam.ofDiscordChannel of
                Just ( filterAtom, channelCache ) ->
                    let
                        ( encodedChannels, guilds ) =
                            prepareDiscordValues channelCache
                    in
                    E.object
                        [ ( "default", Filter.encodeFilterAtom filterAtom )
                        , ( "guilds", E.dict identity Discord.encodeGuild guilds )
                        , ( "channels", E.list identity encodedChannels )
                        ]

                Nothing ->
                    E.null
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


decoder : Decoder FilterAtomMaterial
decoder =
    D.map FilterAtomMaterial
        (D.field "ofDiscordChannel" (D.maybe ofDiscordChannelDecoder))


ofDiscordChannelDecoder : Decoder ( FilterAtom, List Discord.ChannelCache )
ofDiscordChannelDecoder =
    D.do (D.field "default" Filter.filterAtomDecoder) <|
        \fa ->
            D.do (D.field "guilds" (D.dict Discord.guildDecoder)) <|
                \guilds ->
                    D.do (D.field "channels" (D.list (Discord.channelCacheDecoder guilds))) <|
                        \channelCache ->
                            D.succeed ( fa, channelCache )


type UpdateInstruction
    = DiscordInstruction (UpdateFAM ( FilterAtom, List Discord.ChannelCache ))


update : List UpdateInstruction -> FilterAtomMaterial -> FilterAtomMaterial
update instructions fam =
    case instructions of
        [] ->
            fam

        (DiscordInstruction (SetFAM discordFAM)) :: xs ->
            update xs { fam | ofDiscordChannel = Just discordFAM }

        (DiscordInstruction KeepFAM) :: xs ->
            update xs fam

        (DiscordInstruction DestroyFAM) :: xs ->
            update xs { fam | ofDiscordChannel = Nothing }


mapDiscordChannel : String -> FilterAtomMaterial -> (Discord.ChannelCache -> a) -> Maybe a
mapDiscordChannel cId fam mapper =
    fam.ofDiscordChannel
        |> Maybe.andThen (\( _, channels ) -> ListExtra.findOne (.id >> (==) cId) channels)
        |> Maybe.map mapper
