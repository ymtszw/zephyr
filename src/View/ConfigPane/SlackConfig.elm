module View.ConfigPane.SlackConfig exposing (slackConfigEl)

import Data.ColorTheme exposing (aubergine)
import Data.Filter exposing (FilterAtom(..))
import Data.Model exposing (ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.FetchStatus as FetchStatus
import Data.Producer.Slack as Slack exposing (Conversation, Slack(..), SlackRegistry, SlackUnidentified(..), Team, User)
import Data.ProducerRegistry as ProducerRegistry
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (..)
import Octicons
import StringExtra
import Time
import Url
import View.Parts exposing (..)
import View.Select exposing (select)


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
            Dict.foldr (\k v acc -> teamStateKeyEl vs k v :: acc) [] sr.dict

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
        [ textInputEl []
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
            [ el [] (text "New Token")
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


teamStateKeyEl : ViewState -> String -> Slack -> ( String, Element Msg )
teamStateKeyEl vs teamIdStr slack =
    let
        teamStateAttrs =
            [ width fill
            , padding rectElementInnerPadding
            , spacing spacingUnit
            , BD.rounded rectElementRound
            , BD.width 1
            , BD.color aubergine.bd
            ]
    in
    Tuple.pair teamIdStr <|
        column teamStateAttrs <|
            case slack of
                Identified newSession ->
                    [ stateHeaderEl False teamIdStr newSession.team newSession.user ]

                Hydrated _ pov ->
                    [ stateHeaderEl False teamIdStr pov.team pov.user
                    , conversationsEl vs teamIdStr pov.users pov.conversations
                    ]

                Rehydrating _ pov ->
                    [ stateHeaderEl True teamIdStr pov.team pov.user
                    , conversationsEl vs teamIdStr pov.users pov.conversations
                    ]

                Revisit pov ->
                    [ stateHeaderEl True teamIdStr pov.team pov.user
                    , conversationsEl vs teamIdStr pov.users pov.conversations
                    ]

                Expired _ pov ->
                    [ stateHeaderEl False teamIdStr pov.team pov.user
                    , conversationsEl vs teamIdStr pov.users pov.conversations
                    ]


stateHeaderEl : Bool -> String -> Team -> User -> Element Msg
stateHeaderEl rehydrating teamIdStr team user =
    row [ width fill, spacing spacingUnit ]
        [ teamInfoEl team
        , userInfoEl user
        , rehydrateButtonEl rehydrating teamIdStr
        ]


teamInfoEl : Team -> Element Msg
teamInfoEl team =
    row [ width fill, spacing spacingUnit ]
        [ slackTeamIconEl [] identityIconSize team
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


userInfoEl : User -> Element Msg
userInfoEl user =
    row [ width fill, spacing spacingUnit ]
        [ squareIconOrHeadEl []
            { size = identityIconSize
            , name = Maybe.withDefault user.profile.realName user.profile.displayName
            , url = Just (Url.toString user.profile.image48)
            }
        , column [ width shrink, spacing spacingUnit ] <|
            case user.profile.displayName of
                Just dn ->
                    [ el [ Font.bold ] (breakT dn)
                    , el [ Font.size smallFontSize, Font.color aubergine.note ] (breakT user.profile.realName)
                    ]

                Nothing ->
                    [ el [ Font.bold ] (breakT user.profile.realName) ]
        ]


rehydrateButtonEl : Bool -> String -> Element Msg
rehydrateButtonEl rehydrating teamIdStr =
    roundButtonEl [ alignTop, alignRight ]
        { onPress = Slack.IRehydrate teamIdStr
        , enabled = not rehydrating
        , innerElementSize = rehydrateButtonSize
        , innerElement =
            octiconEl [ rotating rehydrating ]
                { size = rehydrateButtonSize
                , color =
                    if rehydrating then
                        defaultOcticonColor

                    else
                        activeRehydrateButtonColor
                , shape = Octicons.sync
                }
        }
        |> mapToRoot


rehydrateButtonSize : Int
rehydrateButtonSize =
    26


activeRehydrateButtonColor : Color
activeRehydrateButtonColor =
    aubergine.prim


conversationsEl : ViewState -> String -> Dict String User -> Dict String Conversation -> Element Msg
conversationsEl vs teamIdStr users conversations =
    Element.Keyed.column [ width fill, spacing spacingUnit, Font.size smallFontSize ] <|
        List.concat <|
            [ [ headerKeyEl ]
            , conversationRows vs teamIdStr users conversations
            ]


headerKeyEl : ( String, Element Msg )
headerKeyEl =
    Tuple.pair "ChannelHeader" <|
        row [ width fill, spacing cellSpacing ]
            [ el [ width fill, BG.color aubergine.note ] <| text "Name"
            , el [ width fill, BG.color aubergine.note ] <| text "Fetch Status"
            , el [ width fill, BG.color aubergine.note ] <| text "Action"
            ]


cellSpacing : Int
cellSpacing =
    2


conversationRows : ViewState -> String -> Dict String User -> Dict String Conversation -> List ( String, Element Msg )
conversationRows vs teamIdStr users conversations =
    let
        ( notSubbedChannels, subbedChannels ) =
            -- TODO support IM/MPIM
            Dict.toList conversations
                |> List.filter (\( _, c ) -> not c.isArchived && Slack.isChannel c)
                |> List.sortWith (\( _, a ) ( _, b ) -> Slack.compareByMembersipThenName a b)
                |> List.partition (\( _, c ) -> FetchStatus.dormant c.fetchStatus)
    in
    subbedChannels
        |> List.map (subbedConversationRowKeyEl vs.timezone teamIdStr)
        |> (::) (subscribeRowKeyEl vs.selectState teamIdStr users notSubbedChannels)


subbedConversationRowKeyEl : Time.Zone -> String -> ( String, Conversation ) -> ( String, Element Msg )
subbedConversationRowKeyEl tz teamIdStr ( convIdStr, conv ) =
    Tuple.pair convIdStr <|
        row [ width fill, spacing cellSpacing, clipX ]
            [ slackConversationEl [ width fill ] { fontSize = smallFontSize, conversation = conv, team = Nothing }
            , el [ width fill ] <| fetchStatusTextEl tz <| conv.fetchStatus
            , row [ width fill, spacing spacingUnit ]
                [ createColumnButtonEl conv
                , unsubscribeButtonEl teamIdStr conv
                ]
            ]


createColumnButtonEl : Conversation -> Element Msg
createColumnButtonEl c =
    thinButtonEl []
        { onPress = AddSimpleColumn (OfSlackConversation (Slack.getConversationIdStr c))
        , width = fill
        , enabledColor = aubergine.prim
        , enabledFontColor = aubergine.text
        , enabled = FetchStatus.subscribed c.fetchStatus
        , innerElement = text "Create Column"
        }


unsubscribeButtonEl : String -> Conversation -> Element Msg
unsubscribeButtonEl teamIdStr conv =
    roundButtonEl [ alignRight ]
        { onPress = Slack.IUnsubscribe teamIdStr (Slack.getConversationIdStr conv)
        , enabled = True -- Any channels show up in table are unsubscribable
        , innerElement =
            octiconEl []
                { size = smallFontSize
                , color = aubergine.err
                , shape = Octicons.circleSlash
                }
        , innerElementSize = smallFontSize
        }
        |> mapToRoot


subscribeRowKeyEl : View.Select.State -> String -> Dict String User -> List ( String, Conversation ) -> ( String, Element Msg )
subscribeRowKeyEl ss teamIdStr users convs =
    [ select [ width (fillPortion 1) ]
        { state = ss
        , msgTagger = SelectCtrl
        , id = conversationSelectId teamIdStr
        , theme = aubergine
        , thin = True
        , onSelect = ProducerCtrl << ProducerRegistry.SlackMsg << Slack.ISubscribe teamIdStr << Slack.getConversationIdStr
        , selectedOption = Nothing
        , filterMatch = Just (\f conv -> StringExtra.containsCaseIgnored f conv.name)
        , options = convs
        , optionEl = \c -> slackConversationEl [] { fontSize = smallFontSize, conversation = c, team = Nothing }
        }
    , el [ width (fillPortion 2) ] none
    ]
        |> row [ width fill ]
        |> Tuple.pair (conversationSelectId teamIdStr)


conversationSelectId : String -> String
conversationSelectId teamIdStr =
    "slackConversationSelect_" ++ teamIdStr
