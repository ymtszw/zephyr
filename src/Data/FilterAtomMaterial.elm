module Data.FilterAtomMaterial exposing (FilterAtomMaterial, UpdateInstruction(..), update)

import Data.Filter exposing (FilterAtom)
import Data.Producer.Base exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, List Discord.ChannelCache ) -- List instead of Dict, should be sorted already
    }


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
