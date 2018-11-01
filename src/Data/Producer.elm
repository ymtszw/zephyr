module Data.Producer exposing
    ( ProducerRegistry, Msg(..), GrossReload, registryDecoder
    , initRegistry, reloadAll, update
    , Yield, encodeRegistry
    )

{-| Types and functions representing data produecr in Zephyr.


## Types

@docs ProducerRegistry, Msg, GrossReload, Yield encodeRegistry, registryDecoder


## Component APIs

@docs initRegistry, reloadAll, update

-}

import Data.FilterAtomMaterial exposing (UpdateInstruction(..))
import Data.Item exposing (Item(..))
import Data.Producer.Base exposing (PostProcessBase, UpdateFAM(..), YieldBase)
import Data.Producer.Discord as Discord exposing (Discord)
import Json.Decode as D exposing (Decoder, Value)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Worque exposing (Work)



-- MODEL


type alias ProducerRegistry =
    { discord : Maybe Discord
    }


registryDecoder : Decoder ProducerRegistry
registryDecoder =
    D.oneOf
        [ D.map ProducerRegistry (D.maybeField "discord" Discord.decoder)
        , D.map ProducerRegistry oldRegistryDecoder -- Migration
        ]


oldRegistryDecoder : Decoder (Maybe Discord)
oldRegistryDecoder =
    D.list oldProducerDecoder
        |> D.andThen
            (\producers ->
                case producers of
                    discord :: _ ->
                        D.succeed (Just discord)

                    _ ->
                        D.succeed Nothing
            )


oldProducerDecoder : Decoder Discord
oldProducerDecoder =
    D.oneOf
        [ D.tagged "Discord" identity Discord.decoder
        , Discord.decoder
        ]


encodeRegistry : ProducerRegistry -> E.Value
encodeRegistry producerRegistry =
    E.object
        [ ( "discord", E.maybe Discord.encode producerRegistry.discord ) ]


initRegistry : ProducerRegistry
initRegistry =
    { discord = Nothing }



-- RELOAD


type Msg
    = DiscordMsg Discord.Msg


type alias GrossReload =
    { producerRegistry : ProducerRegistry
    , cmd : Cmd Msg
    , famInstructions : List UpdateInstruction
    , works : List Work
    }


{-| Reload all registered Producers on application startup.

Reload function uses same Yield type as return type, but it is just for convenience.
On reload, items are ignored and states are always persisted.

-}
reloadAll : ProducerRegistry -> GrossReload
reloadAll producerRegistry =
    GrossReload producerRegistry Cmd.none [] []
        |> reloadDiscord


reloadDiscord : GrossReload -> GrossReload
reloadDiscord ({ producerRegistry } as gr) =
    case Maybe.map Discord.reload gr.producerRegistry.discord of
        Just y ->
            { gr
                | producerRegistry = { producerRegistry | discord = y.newState }
                , cmd = Cmd.batch [ Cmd.map DiscordMsg y.cmd, gr.cmd ]
                , famInstructions = DiscordInstruction y.postProcess.updateFAM :: gr.famInstructions
                , works =
                    case y.postProcess.work of
                        Just w ->
                            w :: gr.works

                        Nothing ->
                            gr.works
            }

        Nothing ->
            gr



-- UPDATE


type alias Yield =
    { items : List Item
    , postProcess : PostProcess
    , producerRegistry : ProducerRegistry
    , cmd : Cmd Msg
    }


type alias PostProcess =
    { persist : Bool
    , famInstruction : UpdateInstruction
    , work : Maybe Work
    }


{-| Update ProducerRegistry in a manner of component pattern.

Returns same data structure as `receive`.

-}
update : Msg -> ProducerRegistry -> Yield
update msg producerRegistry =
    case msg of
        DiscordMsg dMsg ->
            Discord.update dMsg producerRegistry.discord
                |> mapYield DiscordItem
                    DiscordInstruction
                    DiscordMsg
                    (\newState -> { producerRegistry | discord = newState })


mapYield :
    (item -> Item)
    -> (UpdateFAM mat -> UpdateInstruction)
    -> (msg -> Msg)
    -> (Maybe state -> ProducerRegistry)
    -> YieldBase item mat state msg
    -> Yield
mapYield itemTagger famTagger msgTagger stateSetter y =
    Yield (List.map itemTagger y.items)
        (mapPostProcess famTagger y.postProcess)
        (stateSetter y.newState)
        (Cmd.map msgTagger y.cmd)


mapPostProcess : (UpdateFAM mat -> UpdateInstruction) -> PostProcessBase mat -> PostProcess
mapPostProcess tagger ppb =
    PostProcess ppb.persist (tagger ppb.updateFAM) ppb.work
