module Data.FilterAtomMaterial exposing (FilterAtomMaterial, UpdateInstruction(..), init, mapDiscordChannel, update)

{-| Cache of relatively long-living information used for rendering FilterAtom.
-}

import Data.Filter exposing (FilterAtom)
import Data.Producer.Base exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord
import ListExtra


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, List Discord.ChannelCache ) -- List instead of Dict, should be sorted already
    }


type UpdateInstruction
    = DiscordInstruction (UpdateFAM ( FilterAtom, List Discord.ChannelCache ))


init : FilterAtomMaterial
init =
    { ofDiscordChannel = Nothing }


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
