module Data.FilterAtomMaterial exposing (FilterAtomMaterial, UpdateInstruction, update)

import Data.Filter exposing (FilterAtom)
import Data.Producer.Base exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord


type alias FilterAtomMaterial =
    { ofDiscordChannel : Maybe ( FilterAtom, List Discord.ChannelCache ) -- List instead of Dict, should be sorted already
    }


type alias UpdateInstruction =
    { ofDiscordChannel : UpdateFAM ( FilterAtom, List Discord.ChannelCache )
    }


update : UpdateInstruction -> FilterAtomMaterial -> FilterAtomMaterial
update ins fam =
    case ins.ofDiscordChannel of
        SetFAM discordFAM ->
            { fam | ofDiscordChannel = Just discordFAM }

        KeepFAM ->
            fam

        DestroyFAM ->
            { fam | ofDiscordChannel = Nothing }
