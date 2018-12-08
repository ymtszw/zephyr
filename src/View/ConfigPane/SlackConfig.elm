module View.ConfigPane.SlackConfig exposing (slackConfigEl)

import Data.Model exposing (ViewState)
import Data.Producer.Slack as Slack exposing (Slack, SlackRegistry)
import Element exposing (..)


slackConfigEl : ViewState -> SlackRegistry -> Element msg
slackConfigEl vs slack =
    text "Under Construction"
