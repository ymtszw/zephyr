module Data.Producer.Slack exposing (Slack(..), SlackUnidentified(..), SlackRegistry, User, Team)

{-| Producer for Slack workspaces.

@docs Slack, SlackUnidentified, SlackRegistry, User, Team

-}

import Dict exposing (Dict)
import Url exposing (Url)


{-| A primary state machine of Slack team-identity combination.
-}
type Slack
    = Identified NewSession


type alias NewSession =
    { token : String -- Likely a legacy token, but could be a user token in the future
    , user : User
    , team : Team
    }


{-| A user object.

<https://api.slack.com/types/user>

-}
type alias User =
    { id : UserId
    , name : String
    , teamId : TeamId
    , avatar :
        { image24 : Url
        , image32 : Url
        , image48 : Url
        , image72 : Url
        , image192 : Url
        }
    }


type UserId
    = UserId String


{-| A team object.

<https://api.slack.com/methods/team.info>

-}
type alias Team =
    { id : TeamId
    , name : String
    , domain : String
    , icon :
        { image34 : Url
        , image44 : Url
        , image68 : Url
        , image88 : Url
        , image102 : Url
        , image132 : Url
        , imageDefault : Bool
        }
    }


type TeamId
    = TeamId TeamIdStr


type alias TeamIdStr =
    String


{-| Runtime registry of multiple Slack state machines.
-}
type alias SlackRegistry =
    { dict : Dict TeamIdStr Slack
    , unidentified : SlackUnidentified
    }


{-| Not yet identified token states.
-}
type SlackUnidentified
    = TokenWritable String
    | TokenIdentifying String
