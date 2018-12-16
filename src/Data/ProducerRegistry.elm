module Data.ProducerRegistry exposing
    ( init, decoder, storeId
    , GrossReload, reloadAll, Msg(..), Yield, update
    , ProducerRegistry, encode
    )

{-| Types and functions representing data produecr in Zephyr.

@docs Producer, init, encodeRegistry, decoder, storeId
@docs GrossReload, reloadAll, Msg, Yield, update

-}

import Data.FilterAtomMaterial exposing (UpdateInstruction(..))
import Data.Item exposing (Item(..))
import Data.Producer as Producer exposing (UpdateFAM(..))
import Data.Producer.Discord as Discord exposing (Discord)
import Data.Producer.Slack as Slack exposing (SlackRegistry)
import Data.Storable exposing (Storable)
import Json.Decode as D exposing (Decoder, Value)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
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
    , works : List Work
    }


{-| Reload all registered Producers on application startup.

Reload function uses same Yield type as return type, but it is just for convenience.
On reload, items are ignored and states are always persisted.

-}
reloadAll : ProducerRegistry -> ( ProducerRegistry, GrossReload )
reloadAll producerRegistry =
    ( producerRegistry, { cmd = Cmd.none, famInstructions = [], works = [] } )
        |> reloadImpl DiscordInstruction
            DiscordMsg
            (\d pr -> { pr | discord = d })
            (Discord.reload producerRegistry.discord)
        |> reloadImpl SlackInstruction
            SlackMsg
            (\s pr -> { pr | slack = s })
            (Slack.reload producerRegistry.slack)


reloadImpl :
    (UpdateFAM mat -> UpdateInstruction)
    -> (msg -> Msg)
    -> (state -> ProducerRegistry -> ProducerRegistry)
    -> ( state, Producer.Yield item mat msg )
    -> ( ProducerRegistry, GrossReload )
    -> ( ProducerRegistry, GrossReload )
reloadImpl famTagger msgTagger stateUpdater ( nesState, y ) ( producerRegistry, gr ) =
    ( stateUpdater nesState producerRegistry
    , { cmd = Cmd.batch [ Cmd.map msgTagger y.cmd, gr.cmd ]
      , famInstructions = famTagger y.updateFAM :: gr.famInstructions
      , works =
            case y.work of
                Just w ->
                    w :: gr.works

                Nothing ->
                    gr.works
      }
    )



-- UPDATE


type alias Yield =
    { cmd : Cmd Msg
    , persist : Bool
    , items : List Item
    , famInstruction : UpdateInstruction
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
                |> mapYield DiscordItem
                    DiscordInstruction
                    DiscordMsg
                    (\newState -> { producerRegistry | discord = newState })

        SlackMsg sMsg ->
            Slack.update sMsg producerRegistry.slack
                |> mapYield SlackItem
                    SlackInstruction
                    SlackMsg
                    (\newState -> { producerRegistry | slack = newState })


mapYield :
    (item -> Item)
    -> (UpdateFAM mat -> UpdateInstruction)
    -> (msg -> Msg)
    -> (state -> ProducerRegistry)
    -> ( state, Producer.Yield item mat msg )
    -> ( ProducerRegistry, Yield )
mapYield itemTagger famTagger msgTagger stateSetter ( newState, y ) =
    ( stateSetter newState
    , { cmd = Cmd.map msgTagger y.cmd
      , persist = y.persist
      , items = List.map itemTagger y.items
      , famInstruction = famTagger y.updateFAM
      , work = y.work
      }
    )
