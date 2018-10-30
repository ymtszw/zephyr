module Data.Producer exposing
    ( ProducerRegistry, Msg(..), GrossYield, encodeRegistry, registryDecoder
    , initRegistry, reloadAll, update, configsEl
    )

{-| Types and functions representing data produecr in Zephyr.


## Types

@docs ProducerRegistry, Msg, GrossYield, encodeRegistry, registryDecoder


## Component APIs

@docs initRegistry, reloadAll, update, configsEl

-}

import Data.ColorTheme exposing (oneDark)
import Data.Filter exposing (FilterAtom)
import Data.Item exposing (Item(..))
import Data.Producer.Base exposing (Update, Yield)
import Data.Producer.Discord as Discord exposing (Channel, Discord, Guild)
import Dict exposing (Dict)
import Element exposing (..)
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


{-| Reload all registered Producers on application startup.
-}
reloadAll : ProducerRegistry -> ( ProducerRegistry, Cmd Msg )
reloadAll producerRegistry =
    case Maybe.map Discord.reload producerRegistry.discord of
        Just ( newDiscord, discordCmd ) ->
            ( { producerRegistry | discord = Just newDiscord }, Cmd.map DiscordMsg discordCmd )

        Nothing ->
            ( producerRegistry, Cmd.none )



-- GROSS UPDATE


type Msg
    = DiscordMsg Discord.Msg


type alias GrossYield =
    { items : List Item
    , shouldPersist : Bool
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
            let
                discordYield =
                    Discord.update dMsg producerRegistry.discord
            in
            GrossYield (List.map DiscordItem discordYield.items)
                discordYield.shouldPersist
                { producerRegistry | discord = discordYield.newState }
                (Cmd.map DiscordMsg discordYield.cmd)



-- CONFIG VIEW


configsEl : ProducerRegistry -> Element Msg
configsEl producerRegistry =
    column
        [ width fill
        , padding 10
        , spacingXY 0 20
        , BG.color oneDark.main
        , BD.rounded 10
        , Font.size (scale12 2)
        ]
        [ configWrapEl DiscordMsg "Discord" <| Discord.configEl producerRegistry.discord
        ]


configWrapEl : (msg -> Msg) -> String -> Element msg -> Element Msg
configWrapEl tagger title element =
    map tagger <|
        column [ width fill, spacingXY 0 5 ]
            [ el
                [ width fill
                , Font.bold
                , Font.size (scale12 3)
                , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]
                (text title)
            , element
            ]
