module Data.ProducerRegistry exposing
    ( ProducerRegistry, init, encode, decoder, storeId
    , GrossReload, reloadAll, Msg(..), Yield, update
    , setDiscord, setSlack
    )

{-| Types and functions representing data produecr in Zephyr.

@docs ProducerRegistry, init, encode, decoder, storeId
@docs GrossReload, reloadAll, Msg, Yield, update
@docs setDiscord, setSlack

-}

import Data.ColumnStore.AvailableSources as AvailableSources exposing (AvailableSources)
import Data.FilterAtomMaterial exposing (UpdateInstruction(..))
import Data.Item exposing (Item(..))
import Data.Producer as Producer exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord exposing (Discord)
import Data.Producer.Slack as Slack exposing (SlackRegistry)
import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Worque exposing (Work)



-- MODEL


type alias ProducerRegistry =
    { discord : Discord
    , slack : SlackRegistry
    }


encode : ProducerRegistry -> Storable
encode producerRegistry =
    Data.Storable.encode storeId
        [ ( "discord", Discord.encode producerRegistry.discord )
        , ( "slack", Slack.encodeRegistry producerRegistry.slack )
        ]


storeId : String
storeId =
    "producerRegistry"


decoder : Decoder ProducerRegistry
decoder =
    D.map2 ProducerRegistry
        (D.field "discord" Discord.decoder)
        -- Migration; this is not optional
        (D.optionField "slack" Slack.registryDecoder Slack.initRegistry)


init : ProducerRegistry
init =
    { discord = Discord.init
    , slack = Slack.initRegistry
    }



-- RELOAD


type Msg
    = DiscordMsg Discord.Msg
    | SlackMsg Slack.Msg


type alias GrossReload =
    { cmd : Cmd Msg
    , famInstructions : List UpdateInstruction
    , availableSources : Maybe (AvailableSources -> AvailableSources)
    , works : List Work
    }


{-| Reload all registered Producers on application startup.

Reload function uses same Yield type as return type, but it is just for convenience.
On reload, items are ignored and states are always persisted.

-}
reloadAll : ProducerRegistry -> ( ProducerRegistry, GrossReload )
reloadAll producerRegistry =
    ( producerRegistry, { cmd = Cmd.none, famInstructions = [], availableSources = Nothing, works = [] } )
        |> reloadImpl DiscordInstruction
            AvailableSources.setDiscordChannels
            DiscordMsg
            setDiscord
            (Discord.reload producerRegistry.discord)
        |> reloadImpl SlackInstruction
            AvailableSources.setSlackConvos
            SlackMsg
            setSlack
            (Slack.reload producerRegistry.slack)


reloadImpl :
    (UpdateFAM mat -> UpdateInstruction)
    -> (List s -> AvailableSources -> AvailableSources)
    -> (msg -> Msg)
    -> (state -> ProducerRegistry -> ProducerRegistry)
    -> ( state, Producer.Yield item mat s msg )
    -> ( ProducerRegistry, GrossReload )
    -> ( ProducerRegistry, GrossReload )
reloadImpl famTagger asSetter msgTagger stateUpdater ( nesState, y ) ( producerRegistry, gr ) =
    ( stateUpdater nesState producerRegistry
    , { cmd = Cmd.batch [ Cmd.map msgTagger y.cmd, gr.cmd ]
      , famInstructions = famTagger y.updateFAM :: gr.famInstructions
      , availableSources = transitiveMap asSetter y.availableSources gr.availableSources
      , works =
            case y.work of
                Just w ->
                    w :: gr.works

                Nothing ->
                    gr.works
      }
    )


transitiveMap : (a -> b -> b) -> Maybe a -> Maybe (b -> b) -> Maybe (b -> b)
transitiveMap setter originator acc =
    case originator of
        Just val ->
            case acc of
                Just fun ->
                    Just (fun >> setter val)

                Nothing ->
                    Just (setter val)

        Nothing ->
            acc



-- UPDATE


type alias Yield =
    { cmd : Cmd Msg
    , persist : Bool
    , items : List Item
    , famInstruction : UpdateInstruction
    , updateAvailableSources : Maybe (AvailableSources -> AvailableSources)
    , work : Maybe Work
    }


{-| Update ProducerRegistry in a manner of component pattern.

Returns same data structure as `receive`.

-}
update : Msg -> ProducerRegistry -> ( ProducerRegistry, Yield )
update msg producerRegistry =
    case msg of
        DiscordMsg dMsg ->
            Discord.update dMsg producerRegistry.discord
                |> mapYield producerRegistry
                    DiscordItem
                    DiscordInstruction
                    AvailableSources.setDiscordChannels
                    DiscordMsg
                    setDiscord

        SlackMsg sMsg ->
            Slack.update sMsg producerRegistry.slack
                |> mapYield producerRegistry
                    SlackItem
                    SlackInstruction
                    AvailableSources.setSlackConvos
                    SlackMsg
                    setSlack


mapYield :
    ProducerRegistry
    -> (item -> Item)
    -> (UpdateFAM mat -> UpdateInstruction)
    -> (List s -> AvailableSources -> AvailableSources)
    -> (msg -> Msg)
    -> (state -> ProducerRegistry -> ProducerRegistry)
    -> ( state, Producer.Yield item mat s msg )
    -> ( ProducerRegistry, Yield )
mapYield registry itemTagger famTagger asSetter msgTagger setter ( newState, y ) =
    ( setter newState registry
    , { cmd = Cmd.map msgTagger y.cmd
      , persist = y.persist
      , items = List.map itemTagger y.items
      , famInstruction = famTagger y.updateFAM
      , updateAvailableSources = Maybe.map asSetter y.availableSources
      , work = y.work
      }
    )



-- Accessors


setDiscord : Discord -> ProducerRegistry -> ProducerRegistry
setDiscord val registry =
    { registry | discord = val }


setSlack : SlackRegistry -> ProducerRegistry -> ProducerRegistry
setSlack val registry =
    { registry | slack = val }
