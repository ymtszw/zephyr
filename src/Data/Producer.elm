module Data.Producer exposing
    ( ProducerRegistry, Msg(..), GrossYield, encodeRegistry, registryDecoder
    , initRegistry, reloadAll, update, configsEl
    , discordFilterAtomMaterial
    )

{-| Types and functions representing data produecr in Zephyr.


## Types

@docs ProducerRegistry, Msg, GrossYield, encodeRegistry, registryDecoder


## Component APIs

@docs initRegistry, reloadAll, update, configsEl


## Runtime APIs

@docs discordFilterAtomMaterial

-}

import Data.ColorTheme exposing (oneDark)
import Data.Filter exposing (FilterAtom)
import Data.Item exposing (Item(..))
import Data.Producer.Base exposing (Update, Yield)
import Data.Producer.Discord as Discord exposing (Channel, Discord, Guild)
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Json.Decode as D exposing (Decoder, Value)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Process
import Task
import View.Parts exposing (scale12)



-- MODEL


{-| Runtime dictionary of registered Producers.

Websocket message routing mechanism itself is determined at compile-time,
though messages at runtime are dispatched to respective handlers
ONLY WHEN corresponding Producers are actaully registered.

-}
type alias ProducerRegistry =
    Dict String Producer


type Producer
    = DiscordProducer Discord


registryDecoder : Decoder ProducerRegistry
registryDecoder =
    D.list keyProducerDecoder |> D.map Dict.fromList


keyProducerDecoder : Decoder ( String, Producer )
keyProducerDecoder =
    D.oneOf
        [ D.tagged "Discord" (DiscordProducer >> Tuple.pair "discord") Discord.decoder

        -- Old version; to be removed after migration
        , Discord.decoder |> D.map (DiscordProducer >> Tuple.pair "discord")
        ]


{-| Encode ProducerRegistry.

Notice that we encode Dict into JSArray.
This is to ensure Dict key always properly represents its value Producer.
(e.g. key MUST be "discord" if the JSObject was decoded into DiscordProducer)

-}
encodeRegistry : ProducerRegistry -> E.Value
encodeRegistry producerRegistry =
    producerRegistry
        |> Dict.toList
        |> E.list encodeProducer


encodeProducer : ( String, Producer ) -> E.Value
encodeProducer ( _, producer ) =
    case producer of
        DiscordProducer discord ->
            E.tagged "Discord" (Discord.encode discord)


initRegistry : ProducerRegistry
initRegistry =
    Dict.empty



-- RELOAD


{-| Reload all registered Producers on application startup.
-}
reloadAll : ProducerRegistry -> ( ProducerRegistry, Cmd Msg )
reloadAll producerRegistry =
    Dict.foldl reloadOne ( Dict.empty, Cmd.none ) producerRegistry


reloadOne : String -> Producer -> ( ProducerRegistry, Cmd Msg ) -> ( ProducerRegistry, Cmd Msg )
reloadOne key producer prev =
    -- Currently there is no Realtime Producer
    -- And it is impossible to deregister Producer on reload (reload API not accepting `Maybe state`, just `state`)
    case producer of
        DiscordProducer discord ->
            Discord.reload discord |> saveStateAndBatchCmd key DiscordProducer DiscordMsg prev


saveStateAndBatchCmd :
    String
    -> (state -> Producer)
    -> (msg -> Msg)
    -> ( ProducerRegistry, Cmd Msg )
    -> ( state, Cmd msg )
    -> ( ProducerRegistry, Cmd Msg )
saveStateAndBatchCmd key stateTagger msgTagger ( prevRegistry, prevCmd ) ( state, cmd ) =
    ( Dict.insert key (stateTagger state) prevRegistry, Cmd.batch [ Cmd.map msgTagger cmd, prevCmd ] )



-- GROSS UPDATE


type Msg
    = DiscordMsg Discord.Msg


type alias GrossYield =
    { items : List Item
    , producerRegistry : ProducerRegistry
    , cmd : Cmd Msg
    }


{-| Update ProducerRegistry in a manner of component pattern.

Returns same data structure as `receive`.

-}
update : Msg -> ProducerRegistry -> GrossYield
update msg producerRegistry =
    case msg of
        DiscordMsg dMsg ->
            updateProducer "discord" DiscordItem DiscordProducer DiscordMsg (unwrapDiscord >> Discord.update dMsg) producerRegistry


updateProducer :
    String
    -> (item -> Item)
    -> (state -> Producer)
    -> (msg -> Msg)
    -> (Maybe Producer -> Yield item state msg)
    -> ProducerRegistry
    -> GrossYield
updateProducer key itemTagger stateTagger msgTagger producerUpdate producerRegistry =
    producerRegistry
        |> Dict.get key
        |> producerUpdate
        |> updateProducerRegistry key itemTagger stateTagger msgTagger producerRegistry


updateProducerRegistry :
    String
    -> (item -> Item)
    -> (state -> Producer)
    -> (msg -> Msg)
    -> ProducerRegistry
    -> Yield item state msg
    -> GrossYield
updateProducerRegistry key itemTagger stateTagger msgTagger producerRegistry yield =
    case yield.newState of
        Just state ->
            GrossYield (List.map itemTagger yield.items)
                (Dict.insert key (stateTagger state) producerRegistry)
                (Cmd.map msgTagger yield.cmd)

        Nothing ->
            GrossYield (List.map itemTagger yield.items)
                (Dict.remove key producerRegistry)
                (Cmd.map msgTagger yield.cmd)


unwrapDiscord : Maybe Producer -> Maybe Discord
unwrapDiscord producerMaybe =
    case producerMaybe of
        Just (DiscordProducer discord) ->
            Just discord

        _ ->
            Nothing



-- CONFIG VIEW


configsEl : ProducerRegistry -> Element Msg
configsEl producerRegistry =
    El.column
        [ El.width El.fill
        , El.padding 10
        , El.spacingXY 0 20
        , BG.color oneDark.main
        , BD.rounded 10
        , Font.size (scale12 2)
        ]
        [ Dict.get "discord" producerRegistry
            |> unwrapDiscord
            |> Discord.configEl
            |> configWrapEl DiscordMsg "Discord"
        , configWrapEl never "PH" <|
            El.paragraph [] [ El.text "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]
        ]


configWrapEl : (msg -> Msg) -> String -> Element msg -> Element Msg
configWrapEl tagger title element =
    El.map tagger <|
        El.column [ El.width El.fill, El.spacingXY 0 5 ]
            [ El.el
                [ El.width El.fill
                , Font.bold
                , Font.size (scale12 3)
                , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]
                (El.text title)
            , element
            ]



-- RUNTIME APIs


discordFilterAtomMaterial : ProducerRegistry -> Discord.FilterAtomMaterial
discordFilterAtomMaterial producerRegistry =
    case Dict.get "discord" producerRegistry of
        Just (DiscordProducer discord) ->
            Discord.filterAtomMaterial discord

        _ ->
            Nothing
