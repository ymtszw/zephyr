module Data.Producer exposing
    ( ProducerRegistry, initRegistry, encodeRegistry, registryDecoder, registryStoreId
    , GrossReload, reloadAll, Msg(..), Yield, update
    )

{-| Types and functions representing data produecr in Zephyr.

@docs ProducerRegistry, initRegistry, encodeRegistry, registryDecoder, registryStoreId
@docs GrossReload, reloadAll, Msg, Yield, update

-}

import Data.FilterAtomMaterial exposing (UpdateInstruction(..))
import Data.Item exposing (Item(..))
import Data.Producer.Base exposing (PostProcessBase, UpdateFAM(..), YieldBase)
import Data.Producer.Discord as Discord exposing (Discord)
import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder, Value)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Worque exposing (Work)



-- MODEL


type alias ProducerRegistry =
    { discord : Discord
    }


registryDecoder : Decoder ProducerRegistry
registryDecoder =
    D.map ProducerRegistry (D.field "discord" Discord.decoder)


encodeRegistry : ProducerRegistry -> Storable
encodeRegistry producerRegistry =
    Data.Storable.encode registryStoreId
        [ ( "discord", Discord.encode producerRegistry.discord ) ]


registryStoreId : String
registryStoreId =
    "producerRegistry"


initRegistry : ProducerRegistry
initRegistry =
    { discord = Discord.init
    }



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
    let
        y =
            Discord.reload gr.producerRegistry.discord
    in
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
    -> (state -> ProducerRegistry)
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
