module Data.Producer exposing (Msg(..), ProducerRegistry, Receipt, configsEl, encodeRegistry, engageAll, initRegistry, receive, registryDecoder, update)

import Data.ColorTheme exposing (oneDark)
import Data.Item exposing (Item)
import Data.Producer.Discord as Discord exposing (Discord, Token(..))
import Data.Producer.Realtime exposing (Reply(..))
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as E
import Process
import Task
import Websocket as WS exposing (Endpoint, Frame(..), Key(..))


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
        [ Discord.decoder |> D.map (DiscordProducer >> Tuple.pair "discord")
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
            Discord.encode discord


initRegistry : ProducerRegistry
initRegistry =
    Dict.empty


{-| Engage all registered realtime Producers.
-}
engageAll : WS.State msg -> ProducerRegistry -> ( WS.State msg, Cmd msg )
engageAll wsState producerRegistry =
    Dict.foldl engageOne ( wsState, Cmd.none ) producerRegistry


engageOne : String -> Producer -> ( WS.State msg, Cmd msg ) -> ( WS.State msg, Cmd msg )
engageOne key producer prev =
    case producer of
        DiscordProducer discord ->
            case discord.token of
                Ready _ ->
                    engageAndBatchCmd (Key key) Discord.endpoint prev

                _ ->
                    prev


engageAndBatchCmd : Key -> Endpoint -> ( WS.State msg, Cmd msg ) -> ( WS.State msg, Cmd msg )
engageAndBatchCmd key endpoint ( wsState, prevCmd ) =
    WS.engage key endpoint wsState |> Tuple.mapSecond (\newCmd -> Cmd.batch [ newCmd, prevCmd ])


type alias Receipt msg =
    { producerRegistry : ProducerRegistry
    , wsState : WS.State msg
    , cmd : Cmd msg
    , yields : List Item
    }


{-| Handles Websocket messages for realtime Producers using Websockets.

Realtime Producers must expose Producer.Realtime.Handler type function
that can handle arrived text messages.

-}
receive : (Key -> msg) -> ProducerRegistry -> WS.State msg -> Value -> Receipt msg
receive globalTimeoutTagger producerRegistry wsState val =
    case WS.receive wsState val of
        ( newWsState, MessageFrame key message ) ->
            handleMessageFrame globalTimeoutTagger producerRegistry newWsState key message

        ( newWsState, ControlFrame cmd ) ->
            Receipt producerRegistry newWsState cmd []


handleMessageFrame : (Key -> msg) -> ProducerRegistry -> WS.State msg -> Key -> String -> Receipt msg
handleMessageFrame globalTimeoutTagger producerRegistry wsState (Key key) message =
    case Dict.get key producerRegistry of
        Just producer ->
            dispatch globalTimeoutTagger producer wsState (Key key) message
                |> finalizeReceipt producerRegistry (Key key)

        Nothing ->
            Receipt producerRegistry wsState Cmd.none []


type alias ProducerReceipt msg =
    { producer : Producer
    , wsState : WS.State msg
    , cmd : Cmd msg
    , yields : List Item
    }


dispatch : (Key -> msg) -> Producer -> WS.State msg -> Key -> String -> ProducerReceipt msg
dispatch globalTimeoutTagger producer wsState key message =
    case producer of
        DiscordProducer discord ->
            let
                ( yields, newDiscord, reply ) =
                    Discord.receive discord message

                ( newWsState, cmd ) =
                    handleReply globalTimeoutTagger wsState key Discord.endpoint reply
            in
            ProducerReceipt (DiscordProducer newDiscord) newWsState cmd yields


handleReply : (Key -> msg) -> WS.State msg -> Key -> Endpoint -> Reply -> ( WS.State msg, Cmd msg )
handleReply globalTimeoutTagger wsState key endpoint producerReply =
    case producerReply of
        Reply payload ->
            WS.send wsState key payload

        ReplyWithTimeout payload timeout ->
            WS.send wsState key payload
                |> Tuple.mapSecond (setTimeout (globalTimeoutTagger key) timeout)

        NoReply ->
            ( wsState, Cmd.none )

        Engage ->
            WS.engage key endpoint wsState

        Disengage ->
            WS.disengage wsState key


setTimeout : msg -> Float -> Cmd msg -> Cmd msg
setTimeout timeoutMsg timeout cmd =
    Cmd.batch
        [ cmd
        , Process.sleep timeout |> Task.perform (\_ -> timeoutMsg)
        ]


finalizeReceipt : ProducerRegistry -> Key -> ProducerReceipt msg -> Receipt msg
finalizeReceipt producerRegistry (Key key) { producer, wsState, cmd, yields } =
    Receipt (Dict.insert key producer producerRegistry) wsState cmd yields


type Msg
    = DiscordMsg Discord.Msg
    | Timeout Key


{-| Update ProducerRegistry in a manner of component pattern.

Returns same data structure as `receive`.

-}
update : (Key -> msg) -> Msg -> WS.State msg -> ProducerRegistry -> Receipt msg
update globalTimeoutTagger msg wsState producerRegistry =
    case msg of
        DiscordMsg dMsg ->
            producerRegistry
                |> updateProducer "discord" DiscordProducer (unwrapDiscord >> Discord.update dMsg)
                |> handleUpdateReply globalTimeoutTagger (Key "discord") Discord.endpoint wsState

        Timeout (Key "discord") ->
            producerRegistry
                |> updateProducer "discord" DiscordProducer (unwrapDiscord >> Discord.update Discord.Timeout)
                |> handleUpdateReply globalTimeoutTagger (Key "discord") Discord.endpoint wsState

        otherwise ->
            -- Crash by loop
            update globalTimeoutTagger otherwise wsState producerRegistry


updateProducer : String -> (a -> Producer) -> (Maybe Producer -> ( Maybe a, Reply )) -> ProducerRegistry -> ( ProducerRegistry, Reply )
updateProducer key tagger producerUpdate producerRegistry =
    producerRegistry
        |> Dict.get key
        |> producerUpdate
        |> updateProducerRegistry key tagger producerRegistry


updateProducerRegistry : String -> (a -> Producer) -> ProducerRegistry -> ( Maybe a, Reply ) -> ( ProducerRegistry, Reply )
updateProducerRegistry key tagger producerRegistry ( updateResult, reply ) =
    case updateResult of
        Just state ->
            ( Dict.insert key (tagger state) producerRegistry, reply )

        Nothing ->
            ( Dict.remove key producerRegistry, reply )


handleUpdateReply : (Key -> msg) -> Key -> Endpoint -> WS.State msg -> ( ProducerRegistry, Reply ) -> Receipt msg
handleUpdateReply globalTimeoutTagger key endpoint wsState ( producerRegistry, reply ) =
    let
        ( newWsState, cmd ) =
            handleReply globalTimeoutTagger wsState key endpoint reply
    in
    -- XXX Currently update does not yields Items, but this can change if need be
    Receipt producerRegistry newWsState cmd []


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
        , Font.size 18
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
                , Font.size 24
                , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]
                (El.text title)
            , element
            ]
