module Data.Producer.Slack.Team exposing
    ( Team, Id, encode, decoder, idDecoder
    , getId, getName, getDomain, getIcon, domainUrl
    )

{-| A team object.

@docs Team, Id, TeamIcon, encode, decoder, idDecoder
@docs getId, getName, getDomain, getIcon, domainUrl

-}

import Id
import Json.Decode as D exposing (Decoder)
import Json.DecodeExtra as D
import Json.Encode as E
import Json.EncodeExtra as E
import Url exposing (Url)


{-| A team object.

<https://api.slack.com/methods/team.info>

-}
type Team
    = Team TeamRecord


type alias TeamRecord =
    { id : Id
    , name : String
    , domain : String
    , icon : TeamIcon
    }


type alias TeamIcon =
    { image34 : Url
    , image44 : Url
    , image68 : Url
    , imageDefault : Bool
    }


type alias Id =
    Id.Id String Team


encode : Team -> E.Value
encode (Team team) =
    E.object
        [ ( "id", Id.encode E.string team.id )
        , ( "name", E.string team.name )
        , ( "domain", E.string team.domain )
        , Tuple.pair "icon" <|
            E.object
                [ ( "image_34", E.url team.icon.image34 )
                , ( "image_44", E.url team.icon.image44 )
                , ( "image_68", E.url team.icon.image68 )
                , ( "image_default", E.bool team.icon.imageDefault )
                ]
        ]


decoder : Decoder Team
decoder =
    let
        iconDecoder =
            -- image_default is actually absent when false
            D.map4 TeamIcon
                (D.field "image_34" D.url)
                (D.field "image_44" D.url)
                (D.field "image_68" D.url)
                (D.optionField "image_default" D.bool False)
    in
    D.map Team <|
        D.map4 TeamRecord
            (D.field "id" idDecoder)
            (D.field "name" D.string)
            (D.field "domain" D.string)
            (D.field "icon" iconDecoder)


idDecoder : Decoder Id
idDecoder =
    D.oneOf
        [ Id.decoder D.string
        , --Old format
          D.tagged "TeamId" Id.from D.string
        ]



-- Runtime APIs


getId : Team -> Id
getId (Team team) =
    team.id


getName : Team -> String
getName (Team team) =
    team.name


getDomain : Team -> String
getDomain (Team team) =
    team.domain


getIcon : Team -> TeamIcon
getIcon (Team team) =
    team.icon


domainUrl : String -> Url
domainUrl domain =
    { protocol = Url.Https
    , host = domain ++ ".slack.com"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }
