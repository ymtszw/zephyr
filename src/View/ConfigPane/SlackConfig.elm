module View.ConfigPane.SlackConfig exposing (slackConfigEl)

import Data.ColorTheme exposing (aubergine)
import Data.Model exposing (ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Slack as Slack exposing (Slack(..), SlackRegistry, SlackUnidentified(..))
import Data.ProducerRegistry as ProducerRegistry
import Element exposing (..)
import Element.Font as Font
import Element.Input
import View.Parts exposing (..)


slackConfigEl : ViewState -> SlackRegistry -> Element Msg
slackConfigEl vs sr =
    column
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , Font.color baseFontColor
        ]
        [ newLegacyTokenInputEl sr.unidentified
        ]


baseFontColor : Color
baseFontColor =
    aubergine.text


mapToRoot : Element Slack.Msg -> Element Msg
mapToRoot =
    map (ProducerRegistry.SlackMsg >> ProducerCtrl)


newLegacyTokenInputEl : SlackUnidentified -> Element Msg
newLegacyTokenInputEl u =
    let
        ( text, enabled ) =
            case u of
                TokenWritable token ->
                    ( token, True )

                TokenIdentifying token ->
                    ( token, False )
    in
    column [ width fill, spacing spacingUnit ]
        [ textInputEl
            { onChange = Slack.UTokenInput
            , theme = aubergine
            , enabled = enabled
            , text = text
            , label = newLegacyTokenLabel
            , placeholder = Nothing
            }
        ]
        |> mapToRoot


newLegacyTokenLabel : Element.Input.Label msg
newLegacyTokenLabel =
    Element.Input.labelAbove [] <|
        column [ spacing spacingUnit ]
            [ el [] (text "Legacy Token")
            , paragraph [ Font.color aubergine.note, Font.size smallFontSize ]
                [ text "We use Legacy Token since a Slack App require approval by admins and consume integration counts." ]
            ]


smallFontSize : Int
smallFontSize =
    scale12 1
