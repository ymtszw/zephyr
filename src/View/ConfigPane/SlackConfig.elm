module View.ConfigPane.SlackConfig exposing (slackConfigEl)

import Data.ColorTheme exposing (aubergine)
import Data.Model exposing (ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Slack as Slack exposing (Slack(..), SlackRegistry, SlackUnidentified(..))
import Data.ProducerRegistry as ProducerRegistry
import Dict
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (..)
import Url
import View.Parts exposing (..)


slackConfigEl : ViewState -> SlackRegistry -> Element Msg
slackConfigEl vs sr =
    let
        ( text, enabled ) =
            case sr.unidentified of
                TokenWritable token ->
                    ( token, True )

                TokenIdentifying token ->
                    ( token, False )

        teamStates =
            Dict.foldr (\k v acc -> teamStateKeyEl k v :: acc) [] sr.dict

        newLegacyTokenForm =
            [ newLegacyTokenInputKeyEl enabled text
            , newLegacyTokenSubmitButtonKeyEl enabled
            ]
    in
    Element.Keyed.column
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , Font.color baseFontColor
        ]
        (teamStates ++ newLegacyTokenForm)


baseFontColor : Color
baseFontColor =
    aubergine.text


mapToRoot : Element Slack.Msg -> Element Msg
mapToRoot =
    map (ProducerRegistry.SlackMsg >> ProducerCtrl)


newLegacyTokenInputKeyEl : Bool -> String -> ( String, Element Msg )
newLegacyTokenInputKeyEl enabled text =
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
        |> Tuple.pair "newLegacyTokenInput"


newLegacyTokenLabel : Element.Input.Label msg
newLegacyTokenLabel =
    Element.Input.labelAbove [] <|
        column [ spacing spacingUnit ]
            [ el [] (text "Legacy Token")
            , paragraph [ Font.color aubergine.note, Font.size smallFontSize ]
                [ text "We use "
                , newTabLink [ Font.color aubergine.link ] { url = legacyTokenUrl, label = text "Legacy Token" }
                , text " since a Slack App require approval by admins and consume integration counts."
                ]
            , paragraph [ Font.color aubergine.note, Font.size smallFontSize ]
                [ text "Tokens are stored in IndexedDB of your web browser, and only sent to 'slack.com'. Otherwise it "
                , el [ Font.bold ] (text "never")
                , text " get out of your web browser."
                ]
            ]


legacyTokenUrl : String
legacyTokenUrl =
    "https://api.slack.com/custom-integrations/legacy-tokens"


smallFontSize : Int
smallFontSize =
    scale12 1


newLegacyTokenSubmitButtonKeyEl : Bool -> ( String, Element Msg )
newLegacyTokenSubmitButtonKeyEl submittable =
    primaryButtonEl [ alignRight ]
        { onPress = Slack.UTokenCommit
        , width = shrink
        , theme = aubergine
        , enabled = submittable
        , innerElement =
            if submittable then
                text "Register"

            else
                text "Identifying..."
        }
        |> mapToRoot
        |> Tuple.pair "newLegacyTokenSubmitButton"


teamStateKeyEl : String -> Slack -> ( String, Element Msg )
teamStateKeyEl teamIdStr slack =
    let
        teamStateAttrs =
            [ width fill
            , padding rectElementInnerPadding
            , BD.rounded rectElementRound
            , BD.width 1
            , BD.color aubergine.bd
            ]
    in
    Tuple.pair teamIdStr <|
        column teamStateAttrs <|
            case slack of
                Identified newSession ->
                    [ row [ width fill, spacing spacingUnit ]
                        [ teamInfoEl newSession.team, userInfoEl newSession.user ]
                    ]


teamInfoEl : Slack.Team -> Element Msg
teamInfoEl team =
    row [ width fill, spacing spacingUnit ]
        [ squareIconOrHeadEl [ BG.color aubergine.prim ]
            { size = identityIconSize
            , name = team.name
            , url =
                if team.icon.imageDefault then
                    Nothing

                else
                    Just (Url.toString team.icon.image44)
            }
        , column [ width shrink, spacing spacingUnit ]
            [ el [ Font.bold ] (breakT team.name)
            , let
                teamUrl =
                    Slack.teamUrl team
              in
              newTabLink [ Font.size smallFontSize, Font.color aubergine.link ]
                { url = Url.toString teamUrl, label = breakT teamUrl.host }
            ]
        ]


identityIconSize : Int
identityIconSize =
    40


userInfoEl : Slack.User -> Element Msg
userInfoEl user =
    row [ width fill, spacing spacingUnit ]
        [ squareIconOrHeadEl []
            { size = identityIconSize
            , name = user.profile.displayName
            , url = Just (Url.toString user.profile.image48)
            }
        , column [ width shrink, spacing spacingUnit ]
            [ el [ Font.bold ] (breakT user.profile.displayName)
            , el [ Font.size smallFontSize, Font.color aubergine.note ] (breakT user.profile.realName)
            ]
        ]
